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
  // ������ ��� �������� ����� � �� ������� ��� ����������
  TFileLine = record
    FullLine: string;  // ������ ������
    Key: string;       // ��������� ���� (����� ����� �����)
    Number: Integer;   // �������� ���� (����� �� �����)
  end;

  // ������ �����
  TFileLines = array of TFileLine;

  // ����� ����������
  TSortingThread = class(TThread)
  private
    FOwner: TMainForm;
    FInputFile: string;
    FOutputFile: string;
    FProgress: Integer;
    FStatusMessage: string;
    FTempDir: string;
    FMaxLinesInMemory: Integer; // ������������ ���������� ����� � ������
    FTempFiles: TStringList;    // ������ ��������� ������
    FBufferSize: Integer;       // ������ ������ ��� ������ �����

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

    // ���������� ��� ��������� ����� �� ������ ��������
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
  lblStatus.Caption := '������...';
end;

procedure TMainForm.btnSortClick(Sender: TObject);
begin
  if Trim(edtInputFile.Text) = '' then
  begin
    ShowMessage('����������, ������� ������� ����');
    Exit;
  end;

  if not FileExists(edtInputFile.Text) then
  begin
    ShowMessage('������� ���� �� ����������');
    Exit;
  end;

  if Trim(edtOutputFile.Text) = '' then
  begin
    ShowMessage('����������, ������� �������� ����');
    Exit;
  end;

  FCancelled := False;
  EnableControls(False);
  progressBar.Position := 0;
  lblStatus.Caption := '���������� � ����������...';

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
  // ������������� ���������
end;

procedure TMainForm.OnSortingComplete(Sender: TObject);
begin
  Timer1.Enabled := False;
  UpdateProgress;

  EnableControls(True);
  if FCancelled then
    lblStatus.Caption := '���������� ��������'
  else
    lblStatus.Caption := '���������� ���������';
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
  FStatusMessage := '���������� � ����������...';

  // ������� ��������� ����������
  FTempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
              'FileSorter_' + FormatDateTime('yyyymmddhhnnss', Now);
  ForceDirectories(FTempDir);

  FTempFiles := TStringList.Create;

  // ������������� ������������ ���������� ����� � ������
  FMaxLinesInMemory := 500000; // ����������� ��� ����� ����������� ����������

  // ������������� ������ ������ ��� ������ �����
  FBufferSize := 8192; // 8KB �����
end;

destructor TSortingThread.Destroy;
var
  i: Integer;
begin
  // ������� ��������� �����
  if Assigned(FTempFiles) then
  begin
    for i := 0 to FTempFiles.Count - 1 do
    begin
      try
        if FileExists(FTempFiles[i]) then
          DeleteFile(FTempFiles[i]);
      except
        // ���������� ������ ��� ��������
      end;
    end;
    FTempFiles.Free;
  end;

  // ������� ��������� ����������
  try
    if DirectoryExists(FTempDir) then
      RemoveDir(FTempDir);
  except
    // ���������� ������ ��� ��������
  end;

  inherited;
end;

procedure TSortingThread.Execute;
begin
  try
    // ������������ ����
    ProcessFile;

    // ���� ���� ������� ��������� �����, ���������� ��
    if (FTempFiles.Count > 0) and not FOwner.FCancelled and not Terminated then
    begin
      try
        MergeTempFiles;
      except
        on E: Exception do
        begin
          FStatusMessage := '������ ��� ����������� ������: ' + E.Message;
          Exit;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      FStatusMessage := '������: ' + E.Message;
      FProgress := 0;
    end;
  end;
end;

// ������� ��� ������� ������ �� ������������
function TSortingThread.ParseLine(const Line: string): TFileLine;
var
  DotPos: Integer;
begin
  // �������������� ���������
  Result.FullLine := Line;
  Result.Key := '';
  Result.Number := 0;

  // ������� ������� �����
  DotPos := Pos('.', Line);

  // ���� ����� �������, ��������� ������
  if DotPos > 0 then
  begin
    try
      // ��������� ����� �� �����
      Result.Number := StrToIntDef(Copy(Line, 1, DotPos - 1), 0);
    except
      Result.Number := 0;
    end;

    // ��������� ������ ����� �����
    if DotPos < Length(Line) then
      Result.Key := Copy(Line, DotPos + 1, Length(Line))
    else
      Result.Key := '';
  end
  else
  begin
    // ���� ����� ���, ��� ������ ��������� ������
    Result.Key := Line;
  end;
end;

// ������� ���������� ������� ����� (QuickSort)
procedure TSortingThread.SortLines(var Lines: TFileLines);

  // ��������������� ������� ��� ������ ���������
  procedure Swap(var A, B: TFileLine);
  var
    Temp: TFileLine;
  begin
    Temp := A;
    A := B;
    B := Temp;
  end;

  // ����������� ������� ������� ����������
  procedure QuickSort(var A: TFileLines; iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
    Pivot: TFileLine;
  begin
    // �������� �� ������ ��������
    if FOwner.FCancelled or Terminated then
      Exit;

    Lo := iLo;
    Hi := iHi;

    // �������� ������� ������� �� �������� �������
    Mid := (Lo + Hi) div 2;
    Pivot := A[Mid];

    // ���������� �������
    repeat
      // ���� ������� �����, ������� ������ ��������
      while (CompareText(A[Lo].Key, Pivot.Key) < 0) or
            ((CompareText(A[Lo].Key, Pivot.Key) = 0) and (A[Lo].Number < Pivot.Number)) do
        Inc(Lo);

      // ���� ������� ������, ������� ������ ��������
      while (CompareText(A[Hi].Key, Pivot.Key) > 0) or
            ((CompareText(A[Hi].Key, Pivot.Key) = 0) and (A[Hi].Number > Pivot.Number)) do
        Dec(Hi);

      // ���� ������� �� �����������, ������ �������� �������
      if Lo <= Hi then
      begin
        Swap(A[Lo], A[Hi]);
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    // ���������� ��������� ����� �����
    if Hi > iLo then
      QuickSort(A, iLo, Hi);

    // ���������� ��������� ������ �����
    if Lo < iHi then
      QuickSort(A, Lo, iHi);
  end;

begin
  // ���� ������ ������ ��� �������� ������ ���� �������, �� ��� ������������
  if Length(Lines) <= 1 then
    Exit;

  // ��������� ������� ����������
  QuickSort(Lines, Low(Lines), High(Lines));
end;

// ���������������� �������� ����� �� �����
procedure TSortingThread.LoadLinesFromFile(const FileName: string; var Lines: TFileLines; var LineCount: Integer);
var
  Buffer: TStringList;
  i: Integer;
begin
  Buffer := TStringList.Create;
  try
    // ��������� ���� � �����
    try
      Buffer.LoadFromFile(FileName);
      LineCount := Buffer.Count;

      // �������� ������ �������, ���� ����������
      if Length(Lines) < LineCount then
        SetLength(Lines, LineCount);

      // ������ ������
      for i := 0 to LineCount - 1 do
      begin
        Lines[i] := ParseLine(Buffer[i]);

        // ��������� ������ ��������
        if FOwner.FCancelled or Terminated then
          Break;
      end;
    except
      on E: Exception do
      begin
        FStatusMessage := '������ ��� ������ �����: ' + E.Message;
        LineCount := 0;
      end;
    end;
  finally
    Buffer.Free;
  end;
end;

// ���������� � ���������� ����� �����
procedure TSortingThread.SortAndSaveChunk(var Lines: TFileLines; const OutputFile: string);
var
  OutFile: TextFile;
  i: Integer;
  FileOpened: Boolean;
begin
  FileOpened := False;

  try
    // ��������� ������ �����
    SortLines(Lines);

    // ���� �������� ��������, �������
    if FOwner.FCancelled or Terminated then
      Exit;

    // ��������� ���� ��� ������
    AssignFile(OutFile, OutputFile);
    Rewrite(OutFile);
    FileOpened := True;

    // ���������� ��������������� ������
    for i := Low(Lines) to High(Lines) do
    begin
      WriteLn(OutFile, Lines[i].FullLine);

      // ��������� ������ ��������
      if FOwner.FCancelled or Terminated then
        Break;
    end;
  finally
    // ��������� ����, ���� �� ��� ������
    if FileOpened then
    begin
      try
        CloseFile(OutFile);
      except
        // ���������� ������ ��� ��������
      end;
    end;
  end;
end;

// ��������� �������� �����
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
    // �������� ������ �������� �����
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

    // ��������� ������ ����� � �� ��� ������
    FileSizeMB := FileSize / (1024 * 1024);

    // ���� ���� ���������, ���������� ���������� � ������
    if (FileSize > 0) and (FileSize < 100 * 1024 * 1024) then // < 100MB
    begin
      FStatusMessage := Format('�������� ����� (%.2f ��)...', [FileSizeMB]);

      // ������� ������ ��� �����
      SetLength(Lines, FMaxLinesInMemory);

      // ��������� ���� ���� � ������
      LoadLinesFromFile(FInputFile, Lines, LineCount);

      // ���� ���� ������� ��������
      if LineCount > 0 then
      begin
        // �������� ������ �� ������������ �������
        SetLength(Lines, LineCount);

        // ��������� � ��������� ��������� ����� � �������� ����
        FStatusMessage := Format('���������� ������ (%d �����)...', [LineCount]);
        SortAndSaveChunk(Lines, FOutputFile);

        FProgress := 100;
        FStatusMessage := '���������� ���������';
      end
      else
      begin
        FStatusMessage := '������ ��� �������� ����� ��� ���� ����';
      end;

      // �������, ��� ��� ��� ��� �������
      Exit;
    end;

    // ��� ������� ������ ���������� ���������� �� ������
    FStatusMessage := Format('������ � ���������� ����� (%.2f ��)...', [FileSizeMB]);

    // ������� ����� ��� ������������ ������ �����
    Reader := TStreamReader.Create(FInputFile, TEncoding.Default, True, FBufferSize);
    try
      // �������������� ������ ��� �������� ����� �����
      SetLength(Lines, FMaxLinesInMemory);
      LineCount := 0;

      // ������ ���� ���������
      while not Reader.EndOfStream and not FOwner.FCancelled and not Terminated do
      begin
        Line := Reader.ReadLine;
        FilePos := FilePos + Length(Line) + 2; // +2 ��� CRLF

        // ��������� ������ � ��������� � ������
        Lines[LineCount] := ParseLine(Line);
        Inc(LineCount);

        // ���� ������ ����������, ��������� � ��������� ��� �� ��������� ����
        if LineCount >= FMaxLinesInMemory then
        begin
          // ������� ��������� ����
          TempFileName := FTempDir + '\part_' + IntToStr(ChunkIndex) + '.txt';
          FTempFiles.Add(TempFileName);

          // ��������� � ��������� ����
          FStatusMessage := Format('���������� ����� %d...', [ChunkIndex + 1]);
          SortAndSaveChunk(Lines, TempFileName);

          // ����������� ������ ����� � ���������� ������� �����
          Inc(ChunkIndex);
          LineCount := 0;
        end;

        // ��������� ��������
        if FileSize > 0 then
          FProgress := Trunc((FilePos / FileSize) * 50); // ������ 50% ���������

        // ���� ����������� ���������� ��������� Windows
        if (LineCount mod 10000) = 0 then
          Application.ProcessMessages;
      end;

      // ���� �������� ������������� ������, ��������� ��
      if (LineCount > 0) and not FOwner.FCancelled and not Terminated then
      begin
        // ������� ��������� ����
        TempFileName := FTempDir + '\part_' + IntToStr(ChunkIndex) + '.txt';
        FTempFiles.Add(TempFileName);

        // ��������� � ��������� ����
        FStatusMessage := Format('���������� ����� %d...', [ChunkIndex + 1]);
        SetLength(Lines, LineCount); // �������� ������ �� ������������ �������
        SortAndSaveChunk(Lines, TempFileName);
      end;
    finally
      // ����������� �������
      Reader.Free;
      Lines := nil;
    end;
  except
    on E: Exception do
    begin
      FStatusMessage := '������ ��� ��������� �����: ' + E.Message;
      Lines := nil;
    end;
  end;
end;

// ����������� ��������� ������ � �������������� ����������������� ���������
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
  FStatusMessage := '����������� ��������������� ������...';
  FProgress := 50; // �������� � 50%
  OutFileOpened := False;

  // ���� ��� ������ ��� �����������
  if FTempFiles.Count = 0 then
  begin
    FStatusMessage := '��� ������ ��� �����������';
    Exit;
  end
  else if FTempFiles.Count = 1 then
  begin
    // ������ �������� ������������ ���� � ��������
    try
      CopyFile(PChar(FTempFiles[0]), PChar(FOutputFile), False);
      FProgress := 100;
      FStatusMessage := '���������� ���������';
    except
      on E: Exception do
        FStatusMessage := '������ ��� ����������� �����: ' + E.Message;
    end;
    Exit;
  end;

  // ��������� ����� ���������� �����
  LineCountEstimate := 0;
  Buffer := TStringList.Create;
  try
    for i := 0 to FTempFiles.Count - 1 do
    begin
      try
        // ��������� ������ ������ 100 ����� ��� ������ �������� ������� ������
        Buffer.LoadFromFile(FTempFiles[i]);
        if Buffer.Count > 0 then
        begin
          // ��������� ������ ����� �� ������ �������
          with TFileStream.Create(FTempFiles[i], fmOpenRead or fmShareDenyNone) do
          try
            // ��������� ���������� ����� � �����
            if Buffer.Count > 0 then
            begin
              // ������� ����� ������ � ������
              LineCountEstimate := LineCountEstimate + Size div (Length(Buffer[0]) + 2);
            end;
          finally
            Free;
          end;
        end;
      except
        // ���������� ������ ��� ������
      end;
    end;
  finally
    Buffer.Free;
  end;

  // �������������� �������
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
    // ��������� ��� �����
    for i := 0 to TotalFiles - 1 do
    begin
      try
        AssignFile(Files[i], FTempFiles[i]);
        Reset(Files[i]);
        FilesOpened[i] := True;
        Valid[i] := not Eof(Files[i]);

        // ������ ������ ������ �� ������� �����
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

    // ������� �������� ����
    try
      AssignFile(OutFile, FOutputFile);
      Rewrite(OutFile);
      OutFileOpened := True;

      // ���� ���� ���� �� ���� �������� ����
      while (not FOwner.FCancelled) and (not Terminated) do
      begin
        // ������� ����������� ������� � �������������� ����������������� ������
        MinIndex := -1;

        for i := 0 to TotalFiles - 1 do
        begin
          if Valid[i] and ((MinIndex = -1) or
             (CompareText(ParsedLines[i].Key, ParsedLines[MinIndex].Key) < 0) or
             ((CompareText(ParsedLines[i].Key, ParsedLines[MinIndex].Key) = 0) and
              (ParsedLines[i].Number < ParsedLines[MinIndex].Number))) then
            MinIndex := i;
        end;

        // ���� ��� ������ �������� ������, ������� �� �����
        if MinIndex = -1 then
          Break;

        // ���������� ����������� ������� � �������� ����
        try
          WriteLn(OutFile, Lines[MinIndex]);
          Inc(ProcessedCount);
        except
          // ���������� ������ ��� ������ (DEBUG)
        end;

        // ������ ��������� ������ �� ����� � ����������� ���������
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

        // ��������� ��������
        if (ProcessedCount mod 1000) = 0 then
        begin
          if LineCountEstimate > 0 then
            Progress := 50 + Trunc((ProcessedCount / LineCountEstimate) * 50)
          else
            Progress := 75; // ���� ������ �� �������, ������ ���������� 75%

          if Progress > 99 then
            Progress := 99;

          FProgress := Progress;
          FStatusMessage := Format('����������� ������... %d%%', [Progress]);

          // ���� ����������� ���������� ��������� Windows
          Sleep(1);
        end;
      end;
    except
      on E: Exception do
        FStatusMessage := '������ ��� ����������� ������: ' + E.Message;
    end;
  finally
    // ��������� ��� �����
    if OutFileOpened then
    begin
      try
        CloseFile(OutFile);
      except
        // ���������� ������ ��� ��������
      end;
    end;

    for i := 0 to TotalFiles - 1 do
    begin
      if FilesOpened[i] then
      begin
        try
          CloseFile(Files[i]);
        except
          // ���������� ������ ��� ��������
        end;
      end;
    end;
  end;

  FProgress := 100;
  FStatusMessage := '���������� ���������';
end;

end.
