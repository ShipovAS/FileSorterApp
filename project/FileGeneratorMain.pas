unit FileGeneratorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Math;

type
  TMainForm = class(TForm)
    lblFileSize: TLabel;
    edtFileSize: TEdit;
    cmbSizeUnit: TComboBox;
    lblOutputFile: TLabel;
    edtOutputFile: TEdit;
    btnBrowseOutput: TButton;
    btnGenerate: TButton;
    progressBar: TProgressBar;
    lblStatus: TLabel;
    lblDuplicatePercent: TLabel;
    edtDuplicatePercent: TEdit;
    btnCancel: TButton;
    Timer1: TTimer;
    SaveDialog1: TSaveDialog;
    lblDictionary: TLabel;
    edtDictionary: TEdit;
    btnBrowseDictionary: TButton;
    lblWordsPerLine: TLabel;
    edtWordsPerLine: TEdit;
    OpenDialog1: TOpenDialog;
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnBrowseDictionaryClick(Sender: TObject);
  private
    FCancelled: Boolean;
    FGenerationThread: TThread;
    FProgress: Integer;
    FCurrentPosition: Int64;
    FTotalSize: Int64;
    procedure UpdateProgress;
    procedure EnableControls(Enable: Boolean);
    procedure OnGenerationComplete(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

type
  TGenerationThread = class(TThread)
  private
    FOwner: TMainForm;
    FOutputFile: string;
    FDictionaryFile: string;
    FTotalSize: Int64;
    FDuplicatePercent: Integer;
    FWordsPerLine: Integer;
    FProgress: Integer;
    FCurrentPosition: Int64;
    FWords: TStringList;
    FUniquePhrases: TStringList;
    
    procedure LoadDictionary;
    procedure GenerateFile;
    function GenerateRandomNumber(Min, Max: Integer): Integer;
    function GenerateRandomPhrase(WordCount: Integer): string;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TMainForm; const OutputFile, DictionaryFile: string; 
      TotalSize: Int64; DuplicatePercent, WordsPerLine: Integer);
    destructor Destroy; override;
    property Progress: Integer read FProgress;
    property CurrentPosition: Int64 read FCurrentPosition;
  end;

{ TMainForm }

procedure TMainForm.btnBrowseDictionaryClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtDictionary.Text := OpenDialog1.FileName;
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

procedure TMainForm.btnGenerateClick(Sender: TObject);
var
  FileSize: Int64;
  DuplicatePercent, WordsPerLine: Integer;
begin
  if Trim(edtDictionary.Text) = '' then
  begin
    ShowMessage('����������, ������� ���� �������');
    Exit;
  end;

  if not FileExists(edtDictionary.Text) then
  begin
    ShowMessage('���� ������� �� ����������');
    Exit;
  end;

  if Trim(edtOutputFile.Text) = '' then
  begin
    ShowMessage('����������, ������� �������� ����');
    Exit;
  end;

  try
    FileSize := StrToInt64(edtFileSize.Text);
    case cmbSizeUnit.ItemIndex of
      0: FileSize := FileSize * Int64(1024) * Int64(1024); // MB
      1: FileSize := FileSize * Int64(1024) * Int64(1024) * Int64(1024); // GB
    end;

    if (FileSize < 1 * Int64(1024) * Int64(1024)) or (FileSize > 10 * Int64(1024) * Int64(1024) * Int64(1024)) then
    begin
      ShowMessage('������ ����� ������ ���� �� 1MB �� 10GB');
      Exit;
    end;
  except
    ShowMessage('�������� ������ �����');
    Exit;
  end;

  try
    DuplicatePercent := StrToInt(edtDuplicatePercent.Text);
    if (DuplicatePercent < 0) or (DuplicatePercent > 100) then
    begin
      ShowMessage('������� ���������� ������ ���� �� 0 �� 100');
      Exit;
    end;
  except
    ShowMessage('�������� ������� ����������');
    Exit;
  end;

  try
    WordsPerLine := StrToInt(edtWordsPerLine.Text);
    if (WordsPerLine < 1) or (WordsPerLine > 50) then
    begin
      ShowMessage('���������� ���� � ������ ������ ���� �� 1 �� 50');
      Exit;
    end;
  except
    ShowMessage('�������� ���������� ���� � ������');
    Exit;
  end;

  FTotalSize := FileSize;
  FCancelled := False;
  EnableControls(False);
  progressBar.Position := 0;
  lblStatus.Caption := '�������� �������...';
  
  FGenerationThread := TGenerationThread.Create(Self, edtOutputFile.Text, edtDictionary.Text, 
    FileSize, DuplicatePercent, WordsPerLine);
  FGenerationThread.FreeOnTerminate := True;
  TThread(FGenerationThread).OnTerminate := OnGenerationComplete;
  FGenerationThread.Resume;
  
  Timer1.Enabled := True;
end;

procedure TMainForm.EnableControls(Enable: Boolean);
begin
  edtFileSize.Enabled := Enable;
  cmbSizeUnit.Enabled := Enable;
  edtOutputFile.Enabled := Enable;
  btnBrowseOutput.Enabled := Enable;
  btnGenerate.Enabled := Enable;
  edtDuplicatePercent.Enabled := Enable;
  edtDictionary.Enabled := Enable;
  btnBrowseDictionary.Enabled := Enable;
  edtWordsPerLine.Enabled := Enable;
  btnCancel.Enabled := not Enable;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cmbSizeUnit.Items.Add('MB');
  cmbSizeUnit.Items.Add('GB');
  cmbSizeUnit.ItemIndex := 0;
  
  edtFileSize.Text := '500';
  edtDuplicatePercent.Text := '30';
  edtWordsPerLine.Text := '1';
end;

procedure TMainForm.OnGenerationComplete(Sender: TObject);
begin
  Timer1.Enabled := False;
  UpdateProgress;
  
  EnableControls(True);
  if FCancelled then
    lblStatus.Caption := '��������� ��������'
  else
    lblStatus.Caption := '��������� ���������';
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  UpdateProgress;
end;

procedure TMainForm.UpdateProgress;
var
  GenThread: TGenerationThread;
begin
  if Assigned(FGenerationThread) and (FGenerationThread is TGenerationThread) then
  begin
    GenThread := TGenerationThread(FGenerationThread);
    FProgress := GenThread.Progress;
    FCurrentPosition := GenThread.CurrentPosition;
    
    progressBar.Position := FProgress;
    lblStatus.Caption := Format('��������� �����... %d%% (%d �� �� %d ��)', 
      [FProgress, FCurrentPosition div (1024*1024), FTotalSize div (1024*1024)]);
  end;
end;

{ TGenerationThread }

constructor TGenerationThread.Create(AOwner: TMainForm; const OutputFile, DictionaryFile: string;
  TotalSize: Int64; DuplicatePercent, WordsPerLine: Integer);
begin
  inherited Create(True);
  FOwner := AOwner;
  FOutputFile := OutputFile;
  FDictionaryFile := DictionaryFile;
  FTotalSize := TotalSize;
  FDuplicatePercent := DuplicatePercent;
  FWordsPerLine := WordsPerLine;
  FProgress := 0;
  FCurrentPosition := 0;
  FWords := TStringList.Create;
  FUniquePhrases := TStringList.Create;
end;

destructor TGenerationThread.Destroy;
begin
  FWords.Free;
  FUniquePhrases.Free;
  inherited;
end;

procedure TGenerationThread.Execute;
begin
  try
    // ��������� �������
    LoadDictionary;
    
    // ���� ������� ������� ��������, ���������� ����
    if (FWords.Count > 0) and (not Terminated) and (not FOwner.FCancelled) then
      GenerateFile;
  except
    // ��������� ����������
  end;
end;

procedure TGenerationThread.LoadDictionary;
var
  Buffer: TStringList;
  i, j, Count: Integer;
  Word: string;
begin
  if not FileExists(FDictionaryFile) then
  begin
    FOwner.lblStatus.Caption := '���� ������� �� ������';
    Exit;
  end;
  
  // ��������� �������
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(FDictionaryFile);
    
    // ������������ ������� - ��������� ������ �� ����� � ��������� � ������ ����
    for i := 0 to Buffer.Count - 1 do
    begin
      Word := Trim(Buffer[i]);
      
      // ���� ������ �� ������, ��������� � ��� �����
      if Word <> '' then
        FWords.Add(Word);
      
      // ��������� ������ ��������
      if Terminated or FOwner.FCancelled then
        Break;
    end;
    
    // ���� ������� ������, �������
    if FWords.Count = 0 then
    begin
      FOwner.lblStatus.Caption := '������� ����';
      Exit;
    end;
    
    // ������� ����� ���������� ���� (��� ����������)
    Count := Min(1000, FWords.Count * 10); // ������������ ���������� ���������� ����
    
    for i := 0 to Count - 1 do
    begin
      FUniquePhrases.Add(GenerateRandomPhrase(FWordsPerLine));
      
      // ��������� ������ ��������
      if Terminated or FOwner.FCancelled then
        Break;
    end;
  finally
    Buffer.Free;
  end;
end;

function TGenerationThread.GenerateRandomNumber(Min, Max: Integer): Integer;
begin
  Result := Min + Random(Max - Min + 1);
end;

function TGenerationThread.GenerateRandomPhrase(WordCount: Integer): string;
var
  i, WordIndex: Integer;
  Words: TStringList;
begin
  Words := TStringList.Create;
  try
    // ���������� ��������� ����� �� WordCount ����
    for i := 0 to WordCount - 1 do
    begin
      WordIndex := Random(FWords.Count);
      Words.Add(FWords[WordIndex]);
    end;
    
    // ���������� ����� � �����
    Result := Words.Text;
    // ������� ������ �������� ����� � �������
    Result := StringReplace(Result, #13#10, ' ', [rfReplaceAll]);
    Result := Trim(Result);
  finally
    Words.Free;
  end;
end;

procedure TGenerationThread.GenerateFile;
var
  F: TextFile;
  Line: string;
  Number: Integer;
  i, UpdateStep, LastUpdate: Integer;
  BytesWritten: Int64;
  UseUnique: Boolean;
  PhraseIndex: Integer;
begin
  Randomize;
  
  AssignFile(F, FOutputFile);
  Rewrite(F);
  
  BytesWritten := 0;
  UpdateStep := 1;
  LastUpdate := 0;
  
  while (BytesWritten < FTotalSize) and (not Terminated) and (not FOwner.FCancelled) do
  begin
    // ����������, ������������ �� ���������� ����� ��� �������������
    UseUnique := Random(100) >= FDuplicatePercent;
    
    if UseUnique or (FUniquePhrases.Count = 0) then
      Line := GenerateRandomPhrase(FWordsPerLine)
    else
    begin
      // ���������� ���� �� �������������� ��������������� ����
      PhraseIndex := Random(FUniquePhrases.Count);
      Line := FUniquePhrases[PhraseIndex];
    end;
    
    Number := GenerateRandomNumber(1, 100000);
    Line := IntToStr(Number) + '.' + Line;
    
    WriteLn(F, Line);
    BytesWritten := BytesWritten + Length(Line) + 2; // ��� CRLF
    
    FCurrentPosition := BytesWritten;
    FProgress := Trunc((BytesWritten / FTotalSize) * 100);
    
    // ��������� �������� ������ ��� � 1000 ��������, ����� �� ���������
    if (FProgress >= LastUpdate + UpdateStep) or (FProgress = 100) then
    begin
      LastUpdate := FProgress;
      Application.ProcessMessages;
//      Sleep(1);
    end;
  end;
  
  CloseFile(F);
  
  if FOwner.FCancelled and FileExists(FOutputFile) then
    DeleteFile(FOutputFile);
end;

end.