object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = #1043#1077#1085#1077#1088#1072#1090#1086#1088' '#1090#1077#1089#1090#1086#1074#1099#1093' '#1092#1072#1081#1083#1086#1074
  ClientHeight = 259
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileSize: TLabel
    Left = 16
    Top = 16
    Width = 74
    Height = 13
    Caption = #1056#1072#1079#1084#1077#1088' '#1092#1072#1081#1083#1072':'
  end
  object lblOutputFile: TLabel
    Left = 16
    Top = 75
    Width = 84
    Height = 13
    Caption = #1042#1099#1093#1086#1076#1085#1086#1081' '#1092#1072#1081#1083':'
  end
  object lblStatus: TLabel
    Left = 16
    Top = 195
    Width = 95
    Height = 13
    Caption = #1043#1086#1090#1086#1074' '#1082' '#1075#1077#1085#1077#1088#1072#1094#1080#1080
  end
  object lblDuplicatePercent: TLabel
    Left = 16
    Top = 139
    Width = 111
    Height = 13
    Caption = #1055#1088#1086#1094#1077#1085#1090' '#1076#1091#1073#1083#1080#1082#1072#1090#1086#1074':'
  end
  object lblDictionary: TLabel
    Left = 16
    Top = 43
    Width = 47
    Height = 13
    Caption = #1057#1083#1086#1074#1072#1088#1100':'
  end
  object lblWordsPerLine: TLabel
    Left = 16
    Top = 107
    Width = 137
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1089#1083#1086#1074' '#1074' '#1089#1090#1088#1086#1082#1077':'
  end
  object edtFileSize: TEdit
    Left = 107
    Top = 13
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '500'
  end
  object cmbSizeUnit: TComboBox
    Left = 234
    Top = 13
    Width = 50
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object edtOutputFile: TEdit
    Left = 107
    Top = 72
    Width = 234
    Height = 21
    TabOrder = 2
  end
  object btnBrowseOutput: TButton
    Left = 347
    Top = 70
    Width = 75
    Height = 25
    Caption = #1054#1073#1079#1086#1088'...'
    TabOrder = 3
    OnClick = btnBrowseOutputClick
  end
  object btnGenerate: TButton
    Left = 143
    Top = 163
    Width = 121
    Height = 25
    Caption = #1043#1077#1085#1077#1088#1080#1088#1086#1074#1072#1090#1100
    TabOrder = 4
    OnClick = btnGenerateClick
  end
  object progressBar: TProgressBar
    Left = 16
    Top = 219
    Width = 401
    Height = 17
    TabOrder = 5
  end
  object edtDuplicatePercent: TEdit
    Left = 159
    Top = 126
    Width = 121
    Height = 21
    TabOrder = 6
    Text = '30'
  end
  object btnCancel: TButton
    Left = 270
    Top = 163
    Width = 121
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    Enabled = False
    TabOrder = 7
    OnClick = btnCancelClick
  end
  object edtDictionary: TEdit
    Left = 107
    Top = 40
    Width = 234
    Height = 21
    TabOrder = 8
  end
  object btnBrowseDictionary: TButton
    Left = 347
    Top = 38
    Width = 75
    Height = 25
    Caption = #1054#1073#1079#1086#1088'...'
    TabOrder = 9
    OnClick = btnBrowseDictionaryClick
  end
  object edtWordsPerLine: TEdit
    Left = 159
    Top = 99
    Width = 121
    Height = 21
    TabOrder = 10
    Text = '5'
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 376
    Top = 124
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1077' '#1092#1072#1081#1083#1099' (*.txt)|*.txt|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Title = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1092#1072#1081#1083
    Left = 376
    Top = 172
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1077' '#1092#1072#1081#1083#1099' (*.txt)|*.txt|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Title = #1042#1099#1073#1088#1072#1090#1100' '#1092#1072#1081#1083' '#1089#1083#1086#1074#1072#1088#1103
    Left = 376
    Top = 38
  end
end
