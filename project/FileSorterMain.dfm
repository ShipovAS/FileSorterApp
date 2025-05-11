object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1097#1080#1082' '#1092#1072#1081#1083#1086#1074
  ClientHeight = 169
  ClientWidth = 431
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
  object lblInputFile: TLabel
    Left = 16
    Top = 16
    Width = 76
    Height = 13
    Caption = #1042#1093#1086#1076#1085#1086#1081' '#1092#1072#1081#1083':'
  end
  object lblOutputFile: TLabel
    Left = 16
    Top = 48
    Width = 84
    Height = 13
    Caption = #1042#1099#1093#1086#1076#1085#1086#1081' '#1092#1072#1081#1083':'
  end
  object lblStatus: TLabel
    Left = 16
    Top = 112
    Width = 101
    Height = 13
    Caption = #1043#1086#1090#1086#1074' '#1082' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1077
  end
  object edtInputFile: TEdit
    Left = 107
    Top = 13
    Width = 234
    Height = 21
    TabOrder = 0
  end
  object btnBrowseInput: TButton
    Left = 347
    Top = 11
    Width = 75
    Height = 25
    Caption = #1054#1073#1079#1086#1088'...'
    TabOrder = 1
    OnClick = btnBrowseInputClick
  end
  object edtOutputFile: TEdit
    Left = 107
    Top = 45
    Width = 234
    Height = 21
    TabOrder = 2
  end
  object btnBrowseOutput: TButton
    Left = 347
    Top = 43
    Width = 75
    Height = 25
    Caption = #1054#1073#1079#1086#1088'...'
    TabOrder = 3
    OnClick = btnBrowseOutputClick
  end
  object btnSort: TButton
    Left = 171
    Top = 80
    Width = 121
    Height = 25
    Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100
    TabOrder = 4
    OnClick = btnSortClick
  end
  object progressBar: TProgressBar
    Left = 16
    Top = 136
    Width = 401
    Height = 17
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 298
    Top = 80
    Width = 121
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    Enabled = False
    TabOrder = 6
    OnClick = btnCancelClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 80
    Top = 72
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1077' '#1092#1072#1081#1083#1099' (*.txt)|*.txt|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Title = #1042#1099#1073#1077#1088#1080#1090#1077' '#1092#1072#1081#1083' '#1076#1083#1103' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080
    Left = 16
    Top = 72
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1077' '#1092#1072#1081#1083#1099' (*.txt)|*.txt|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Title = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1086#1090#1089#1086#1088#1090#1080#1088#1086#1074#1072#1085#1085#1099#1081' '#1092#1072#1081#1083
    Left = 48
    Top = 72
  end
end
