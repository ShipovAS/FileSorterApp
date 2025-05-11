unit FileSorterMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    lblInputFile: TLabel;
    edtInputFile: TEdit;
    btnBrowseInput: TButton;
    lblOutputFile: TLabel;
    edtOutputFile: TEdit;
    btnBrowseOutput: TButton;
    btnSort: TButton;
    progressBar: TProgressBar;
    lblStatus: TLabel;
    btnCancel: TButton;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCancelled: Boolean;
    FSortingThread: TThread;
    FProgress: Integer;
    FStatusMessage: string;
    procedure UpdateProgress;
    procedure EnableControls(Enable: Boolean);
    procedure OnSortingComplete(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

type
  // Запись для хранения строк с их ключами для сортировки
  TFileLine = record
    FullLine: string;  // Полная строка
    Key: string;       // Строковый ключ (часть после точки)
    Number: Integer;   // Числовой ключ (часть до точки)
  end;

  // Массив строк
  TFileLines = array of TFileLine;

  // Поток сортировки
  TSortingThread = class(TThread)
  private
    FOwner: TMainForm;
    FInputFile: string;
    FOutputFile: string;
    FProgress: Integer;
    FStatusMessage: string;
    FTempDir: string;
    FMaxLinesInMemory: Integer; // Максимальное количество строк в памяти
    FTempFiles: TStringList;    // Список временных файлов
    FBufferSize: Integer;       // Размер буфера для чтения файла

    procedure ProcessFile;
    procedure SortAndSaveChunk(var Lines: TFileLines; const OutputFile: string);
    procedure MergeTempFiles;
    function ParseLine(const Line: string): TFileLine;
    procedure SortLines(var Lines: TFileLines);
    procedure LoadLinesFromFile(const FileName: string; var Lines: TFileLines; var LineCount: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TMainForm; const InputFile, OutputFile: string);
    destructor Destroy; override;
    property Progress: Integer read FProgress;
    property StatusMessage: string read FStatusMessage;
  end;

{ TMainForm }

procedure TMainForm.btnBrowseInputClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edtInputFile.Text := OpenDialog1.FileName;

    // Предлагаем имя выходного файла на основе входного
    if edtOutputFile.Text = '' then
    begin
      edtOutputFile.Text := ChangeFileExt(OpenDialog1.FileName, '') + '_sorted.txt';
    end;
  end;
end;

procedure TMainForm.btnBrowseOutputClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    edtOutputFile.Text := SaveDialog1.FileName;
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
  FCancelled := True;
  btnCancel.Enabled := False;
  lblStatus.Caption := 'Отмена...';
end;

procedure TMainForm.btnSortClick(Sender: TObject);
begin
  if Trim(edtInputFile.Text) = '' then
  begin
    ShowMessage('Пожалуйста, укажите входной файл');
    Exit;
  end;

  if not FileExists(edtInputFile.Text) then
  begin
    ShowMessage('Входной файл не существует');
    Exit;
  end;

  if Trim(edtOutputFile.Text) = '' then
  begin
    ShowMessage('Пожалуйста, укажите выходной файл');
    Exit;
  end;

  FCancelled := False;
  EnableControls(False);
  progressBar.Position := 0;
  lblStatus.Caption := 'Подготовка к сортировке...';

  FSortingThread := TSortingThread.Create(Self, edtInputFile.Text, edtOutputFile.Text);
  FSortingThread.FreeOnTerminate := True;
  TThread(FSortingThread).OnTerminate := OnSortingComplete;
  FSortingThread.Resume;

  Timer1.Enabled := True;
end;

procedure TMainForm.EnableControls(Enable: Boolean);
begin
  edtInputFile.Enabled := Enable;
  btnBrowseInput.Enabled := Enable;
  edtOutputFile.Enabled := Enable;
  btnBrowseOutput.Enabled := Enable;
  btnSort.Enabled := Enable;
  btnCancel.Enabled := not Enable;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Инициализация контролов
end;

procedure TMainForm.OnSortingComplete(Sender: TObject);
begin
  Timer1.Enabled := False;
  UpdateProgress;

  EnableControls(True);
  if FCancelled then
    lblStatus.Caption := 'Сортировка отменена'
  else
    lblStatus.Caption := 'Сортировка завершена';
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  UpdateProgress;
end;

procedure TMainForm.UpdateProgress;
var
  SortThread: TSortingThread;
begin
  if Assigned(FSortingThread) and (FSortingThread is TSortingThread) then
  begin
    SortThread := TSortingThread(FSortingThread);
    FProgress := SortThread.Progress;

    progressBar.Position := FProgress;
    lblStatus.Caption := SortThread.StatusMessage;
  end;
end;

{ TSortingThread }

constructor TSortingThread.Create(AOwner: TMainForm; const InputFile, OutputFile: string);
begin
  inherited Create(True);
  FOwner := AOwner;
  FInputFile := InputFile;
  FOutputFile := OutputFile;
  FProgress := 0;
  FStatusMessage := 'Подготовка к сортировке...';

  // Создаем временную директорию
  FTempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
              'FileSorter_' + FormatDateTime('yyyymmddhhnnss', Now);
  ForceDirectories(FTempDir);

  FTempFiles := TStringList.Create;

  // Устанавливаем максимальное количество строк в памяти
  FMaxLinesInMemory := 500000; // Увеличиваем для более эффективной сортировки

  // Устанавливаем размер буфера для чтения файла
  FBufferSize := 8192; // 8KB буфер
end;

destructor TSortingThread.Destroy;
var
  i: Integer;
begin
  // Удаляем временные файлы
  if Assigned(FTempFiles) then
  begin
    for i := 0 to FTempFiles.Count - 1 do
    begin
      try
        if FileExists(FTempFiles[i]) then
          DeleteFile(FTempFiles[i]);
      except
        // Игнорируем ошибки при удалении
      end;
    end;
    FTempFiles.Free;
  end;

  // Удаляем временную директорию
  try
    if DirectoryExists(FTempDir) then
      RemoveDir(FTempDir);
  except
    // Игнорируем ошибки при удалении
  end;

  inherited;
end;

procedure TSortingThread.Execute;
begin
  try
    // Обрабатываем файл
    ProcessFile;

    // Если были созданы временные файлы, объединяем их
    if (FTempFiles.Count > 0) and not FOwner.FCancelled and not Terminated then
    begin
      try
        MergeTempFiles;
      except
        on E: Exception do
        begin
          FStatusMessage := 'Ошибка при объединении файлов: ' + E.Message;
          Exit;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      FStatusMessage := 'Ошибка: ' + E.Message;
      FProgress := 0;
    end;
  end;
end;

// Функция для разбора строки на составляющие
function TSortingThread.ParseLine(const Line: string): TFileLine;
var
  DotPos: Integer;
begin
  // Инициализируем результат
  Result.FullLine := Line;
  Result.Key := '';
  Result.Number := 0;

  // Находим позицию точки
  DotPos := Pos('.', Line);

  // Если точка найдена, разбираем строку
  if DotPos > 0 then
  begin
    try
      // Извлекаем число до точки
      Result.Number := StrToIntDef(Copy(Line, 1, DotPos - 1), 0);
    except
      Result.Number := 0;
    end;

    // Извлекаем строку после точки
    if DotPos < Length(Line) then
      Result.Key := Copy(Line, DotPos + 1, Length(Line))
    else
      Result.Key := '';
  end
  else
  begin
    // Если точки нет, вся строка считается ключом
    Result.Key := Line;
  end;
end;

// Быстрая сортировка массива строк (QuickSort)
procedure TSortingThread.SortLines(var Lines: TFileLines);

  // Вспомогательная функция для обмена элементов
  procedure Swap(var A, B: TFileLine);
  var
    Temp: TFileLine;
  begin
    Temp := A;
    A := B;
    B := Temp;
  end;

  // Рекурсивная функция быстрой сортировки
  procedure QuickSort(var A: TFileLines; iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
    Pivot: TFileLine;
  begin
    // Проверка на отмену операции
    if FOwner.FCancelled or Terminated then
      Exit;

    Lo := iLo;
    Hi := iHi;

    // Выбираем опорный элемент из середины массива
    Mid := (Lo + Hi) div 2;
    Pivot := A[Mid];

    // Разделение массива
    repeat
      // Ищем элемент слева, который больше опорного
      while (CompareText(A[Lo].Key, Pivot.Key) < 0) or
            ((CompareText(A[Lo].Key, Pivot.Key) = 0) and (A[Lo].Number < Pivot.Number)) do
        Inc(Lo);

      // Ищем элемент справа, который меньше опорного
      while (CompareText(A[Hi].Key, Pivot.Key) > 0) or
            ((CompareText(A[Hi].Key, Pivot.Key) = 0) and (A[Hi].Number > Pivot.Number)) do
        Dec(Hi);

      // Если индексы не пересеклись, меняем элементы местами
      if Lo <= Hi then
      begin
        Swap(A[Lo], A[Hi]);
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    // Рекурсивно сортируем левую часть
    if Hi > iLo then
      QuickSort(A, iLo, Hi);

    // Рекурсивно сортируем правую часть
    if Lo < iHi then
      QuickSort(A, Lo, iHi);
  end;

begin
  // Если массив пустой или содержит только один элемент, он уже отсортирован
  if Length(Lines) <= 1 then
    Exit;

  // Запускаем быструю сортировку
  QuickSort(Lines, Low(Lines), High(Lines));
end;

// Оптимизированная загрузка строк из файла
procedure TSortingThread.LoadLinesFromFile(const FileName: string; var Lines: TFileLines; var LineCount: Integer);
var
  Buffer: TStringList;
  i: Integer;
begin
  Buffer := TStringList.Create;
  try
    // Загружаем файл в буфер
    try
      Buffer.LoadFromFile(FileName);
      LineCount := Buffer.Count;

      // Изменяем размер массива, если необходимо
      if Length(Lines) < LineCount then
        SetLength(Lines, LineCount);

      // Парсим строки
      for i := 0 to LineCount - 1 do
      begin
        Lines[i] := ParseLine(Buffer[i]);

        // Проверяем отмену операции
        if FOwner.FCancelled or Terminated then
          Break;
      end;
    except
      on E: Exception do
      begin
        FStatusMessage := 'Ошибка при чтении файла: ' + E.Message;
        LineCount := 0;
      end;
    end;
  finally
    Buffer.Free;
  end;
end;

// Сортировка и сохранение блока строк
procedure TSortingThread.SortAndSaveChunk(var Lines: TFileLines; const OutputFile: string);
var
  OutFile: TextFile;
  i: Integer;
  FileOpened: Boolean;
begin
  FileOpened := False;

  try
    // Сортируем массив строк
    SortLines(Lines);

    // Если операция отменена, выходим
    if FOwner.FCancelled or Terminated then
      Exit;

    // Открываем файл для записи
    AssignFile(OutFile, OutputFile);
    Rewrite(OutFile);
    FileOpened := True;

    // Записываем отсортированные строки
    for i := Low(Lines) to High(Lines) do
    begin
      WriteLn(OutFile, Lines[i].FullLine);

      // Проверяем отмену операции
      if FOwner.FCancelled or Terminated then
        Break;
    end;
  finally
    // Закрываем файл, если он был открыт
    if FileOpened then
    begin
      try
        CloseFile(OutFile);
      except
        // Игнорируем ошибки при закрытии
      end;
    end;
  end;
end;

// Обработка входного файла
procedure TSortingThread.ProcessFile;
var
  FileSize, FilePos: Int64;
  Lines: TFileLines;
  LineCount, ChunkIndex: Integer;
  TempFileName: string;
  FileStream: TFileStream;
  Reader: TStreamReader;
  Line: string;
  FileSizeMB: Double;
begin
  ChunkIndex := 0;
  LineCount := 0;
  FilePos := 0;

  try
    // Получаем размер входного файла
    FileSize := 0;
    try
      FileStream := TFileStream.Create(FInputFile, fmOpenRead or fmShareDenyNone);
      try
        FileSize := FileStream.Size;
      finally
        FileStream.Free;
      end;
    except
      FileSize := 0;
    end;

    // Вычисляем размер файла в МБ для вывода
    FileSizeMB := FileSize / (1024 * 1024);

    // Если файл маленький, используем сортировку в памяти
    if (FileSize > 0) and (FileSize < 100 * 1024 * 1024) then // < 100MB
    begin
      FStatusMessage := Format('Загрузка файла (%.2f МБ)...', [FileSizeMB]);

      // Создаем массив для строк
      SetLength(Lines, FMaxLinesInMemory);

      // Загружаем весь файл в память
      LoadLinesFromFile(FInputFile, Lines, LineCount);

      // Если файл успешно загружен
      if LineCount > 0 then
      begin
        // Обрезаем массив до фактического размера
        SetLength(Lines, LineCount);

        // Сортируем и сохраняем результат сразу в выходной файл
        FStatusMessage := Format('Сортировка данных (%d строк)...', [LineCount]);
        SortAndSaveChunk(Lines, FOutputFile);

        FProgress := 100;
        FStatusMessage := 'Сортировка завершена';
      end
      else
      begin
        FStatusMessage := 'Ошибка при загрузке файла или файл пуст';
      end;

      // Выходим, так как все уже сделано
      Exit;
    end;

    // Для больших файлов используем сортировку по частям
    FStatusMessage := Format('Чтение и разделение файла (%.2f МБ)...', [FileSizeMB]);

    // Создаем ридер для эффективного чтения файла
    Reader := TStreamReader.Create(FInputFile, TEncoding.Default, True, FBufferSize);
    try
      // Подготавливаем массив для хранения блока строк
      SetLength(Lines, FMaxLinesInMemory);
      LineCount := 0;

      // Читаем файл построчно
      while not Reader.EndOfStream and not FOwner.FCancelled and not Terminated do
      begin
        Line := Reader.ReadLine;
        FilePos := FilePos + Length(Line) + 2; // +2 для CRLF

        // Разбираем строку и добавляем в массив
        Lines[LineCount] := ParseLine(Line);
        Inc(LineCount);

        // Если массив заполнился, сортируем и сохраняем его во временный файл
        if LineCount >= FMaxLinesInMemory then
        begin
          // Создаем временный файл
          TempFileName := FTempDir + '\part_' + IntToStr(ChunkIndex) + '.txt';
          FTempFiles.Add(TempFileName);

          // Сортируем и сохраняем блок
          FStatusMessage := Format('Сортировка части %d...', [ChunkIndex + 1]);
          SortAndSaveChunk(Lines, TempFileName);

          // Увеличиваем индекс части и сбрасываем счетчик строк
          Inc(ChunkIndex);
          LineCount := 0;
        end;

        // Обновляем прогресс
        if FileSize > 0 then
          FProgress := Trunc((FilePos / FileSize) * 50); // Первые 50% прогресса

        // Даем возможность обработать сообщения Windows
        if (LineCount mod 10000) = 0 then
          Application.ProcessMessages;
      end;

      // Если остались несохраненные строки, сохраняем их
      if (LineCount > 0) and not FOwner.FCancelled and not Terminated then
      begin
        // Создаем временный файл
        TempFileName := FTempDir + '\part_' + IntToStr(ChunkIndex) + '.txt';
        FTempFiles.Add(TempFileName);

        // Сортируем и сохраняем блок
        FStatusMessage := Format('Сортировка части %d...', [ChunkIndex + 1]);
        SetLength(Lines, LineCount); // Обрезаем массив до фактического размера
        SortAndSaveChunk(Lines, TempFileName);
      end;
    finally
      // Освобождаем ресурсы
      Reader.Free;
      Lines := nil;
    end;
  except
    on E: Exception do
    begin
      FStatusMessage := 'Ошибка при обработке файла: ' + E.Message;
      Lines := nil;
    end;
  end;
end;

// Объединение временных файлов с использованием оптимизированного алгоритма
procedure TSortingThread.MergeTempFiles;
var
  OutFile: TextFile;
  Files: array of TextFile;
  Lines: array of string;
  ParsedLines: array of TFileLine;
  Valid: array of Boolean;
  i, MinIndex, TotalFiles, ProcessedCount, TotalCount: Integer;
  Progress: Integer;
  OutFileOpened: Boolean;
  FilesOpened: array of Boolean;
  Buffer: TStringList;
  LineCountEstimate: Int64;
begin
  FStatusMessage := 'Объединение отсортированных частей...';
  FProgress := 50; // Начинаем с 50%
  OutFileOpened := False;

  // Если нет файлов для объединения
  if FTempFiles.Count = 0 then
  begin
    FStatusMessage := 'Нет частей для объединения';
    Exit;
  end
  else if FTempFiles.Count = 1 then
  begin
    // Просто копируем единственный файл в выходной
    try
      CopyFile(PChar(FTempFiles[0]), PChar(FOutputFile), False);
      FProgress := 100;
      FStatusMessage := 'Сортировка завершена';
    except
      on E: Exception do
        FStatusMessage := 'Ошибка при копировании файла: ' + E.Message;
    end;
    Exit;
  end;

  // Оцениваем общее количество строк
  LineCountEstimate := 0;
  Buffer := TStringList.Create;
  try
    for i := 0 to FTempFiles.Count - 1 do
    begin
      try
        // Загружаем только первые 100 строк для оценки среднего размера строки
        Buffer.LoadFromFile(FTempFiles[i]);
        if Buffer.Count > 0 then
        begin
          // Оцениваем размер файла по первым строкам
          with TFileStream.Create(FTempFiles[i], fmOpenRead or fmShareDenyNone) do
          try
            // Оцениваем количество строк в файле
            if Buffer.Count > 0 then
            begin
              // Средняя длина строки в байтах
              LineCountEstimate := LineCountEstimate + Size div (Length(Buffer[0]) + 2);
            end;
          finally
            Free;
          end;
        end;
      except
        // Игнорируем ошибки при оценке
      end;
    end;
  finally
    Buffer.Free;
  end;

  // Инициализируем массивы
  TotalFiles := FTempFiles.Count;
  SetLength(Files, TotalFiles);
  SetLength(FilesOpened, TotalFiles);
  SetLength(Lines, TotalFiles);
  SetLength(ParsedLines, TotalFiles);
  SetLength(Valid, TotalFiles);

  for i := 0 to TotalFiles - 1 do
    FilesOpened[i] := False;

  ProcessedCount := 0;

  try
    // Открываем все файлы
    for i := 0 to TotalFiles - 1 do
    begin
      try
        AssignFile(Files[i], FTempFiles[i]);
        Reset(Files[i]);
        FilesOpened[i] := True;
        Valid[i] := not Eof(Files[i]);

        // Читаем первую строку из каждого файла
        if Valid[i] then
        begin
          try
            ReadLn(Files[i], Lines[i]);
            ParsedLines[i] := ParseLine(Lines[i]);
          except
            Valid[i] := False;
          end;
        end;
      except
        Valid[i] := False;
      end;
    end;

    // Создаем выходной файл
    try
      AssignFile(OutFile, FOutputFile);
      Rewrite(OutFile);
      OutFileOpened := True;

      // Пока есть хотя бы один валидный файл
      while (not FOwner.FCancelled) and (not Terminated) do
      begin
        // Находим минимальный элемент с использованием оптимизированного поиска
        MinIndex := -1;

        for i := 0 to TotalFiles - 1 do
        begin
          if Valid[i] and ((MinIndex = -1) or
             (CompareText(ParsedLines[i].Key, ParsedLines[MinIndex].Key) < 0) or
             ((CompareText(ParsedLines[i].Key, ParsedLines[MinIndex].Key) = 0) and
              (ParsedLines[i].Number < ParsedLines[MinIndex].Number))) then
            MinIndex := i;
        end;

        // Если нет больше валидных файлов, выходим из цикла
        if MinIndex = -1 then
          Break;

        // Записываем минимальный элемент в выходной файл
        try
          WriteLn(OutFile, Lines[MinIndex]);
          Inc(ProcessedCount);
        except
          // Игнорируем ошибки при записи (DEBUG)
        end;

        // Читаем следующую строку из файла с минимальным элементом
        try
          if not Eof(Files[MinIndex]) then
          begin
            try
              ReadLn(Files[MinIndex], Lines[MinIndex]);
              ParsedLines[MinIndex] := ParseLine(Lines[MinIndex]);
            except
              Valid[MinIndex] := False;
            end;
          end
          else
            Valid[MinIndex] := False;
        except
          Valid[MinIndex] := False;
        end;

        // Обновляем прогресс
        if (ProcessedCount mod 1000) = 0 then
        begin
          if LineCountEstimate > 0 then
            Progress := 50 + Trunc((ProcessedCount / LineCountEstimate) * 50)
          else
            Progress := 75; // Если оценка не удалась, просто показываем 75%

          if Progress > 99 then
            Progress := 99;

          FProgress := Progress;
          FStatusMessage := Format('Объединение частей... %d%%', [Progress]);

          // Даем возможность обработать сообщения Windows
          Sleep(1);
        end;
      end;
    except
      on E: Exception do
        FStatusMessage := 'Ошибка при объединении файлов: ' + E.Message;
    end;
  finally
    // Закрываем все файлы
    if OutFileOpened then
    begin
      try
        CloseFile(OutFile);
      except
        // Игнорируем ошибки при закрытии
      end;
    end;

    for i := 0 to TotalFiles - 1 do
    begin
      if FilesOpened[i] then
      begin
        try
          CloseFile(Files[i]);
        except
          // Игнорируем ошибки при закрытии
        end;
      end;
    end;
  end;

  FProgress := 100;
  FStatusMessage := 'Сортировка завершена';
end;

end.
