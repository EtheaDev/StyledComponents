object MainForm: TMainForm
  Left = 0
  Top = 0
  HelpContext = 100
  Caption = 'StyledTaskDialog Demo (c) Ethea S.r.l.'
  ClientHeight = 496
  ClientWidth = 731
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DefaultButtonLabel: TLabel
    Left = 484
    Top = 367
    Width = 70
    Height = 13
    Alignment = taRightJustify
    Caption = 'Default Button'
  end
  object TitleLabel: TLabel
    Left = 8
    Top = 7
    Width = 24
    Height = 13
    Caption = 'Title:'
  end
  object MessageLabel: TLabel
    Left = 8
    Top = 49
    Width = 46
    Height = 13
    Caption = 'Message:'
  end
  object edTitle: TEdit
    Left = 8
    Top = 24
    Width = 434
    Height = 21
    TabOrder = 0
    Text = 'Task dialog Tester'
  end
  object edMessage: TMemo
    Left = 8
    Top = 64
    Width = 434
    Height = 294
    TabOrder = 1
    WordWrap = False
  end
  object btTask: TButton
    Left = 16
    Top = 387
    Width = 184
    Height = 25
    Caption = 'Show Styled Task Dialog'
    TabOrder = 2
    OnClick = ShowDlg
  end
  object rgDlgType: TRadioGroup
    Left = 448
    Top = 16
    Width = 273
    Height = 105
    Caption = 'Dialog Type'
    TabOrder = 3
  end
  object clbButtons: TCheckListBox
    Left = 448
    Top = 139
    Width = 273
    Height = 219
    ItemHeight = 13
    TabOrder = 4
  end
  object btMsg: TButton
    Left = 16
    Top = 463
    Width = 184
    Height = 25
    Caption = 'Show Message Dialog'
    TabOrder = 5
    OnClick = ShowDlg
  end
  object btError: TButton
    Left = 537
    Top = 463
    Width = 184
    Height = 25
    Caption = 'Raise Error with Task Dialog'
    TabOrder = 6
    OnClick = RaiseError
  end
  object DefaultButtonComboBox: TComboBox
    Left = 560
    Top = 364
    Width = 161
    Height = 21
    Style = csDropDownList
    TabOrder = 7
  end
  object btStdTask: TButton
    Left = 206
    Top = 387
    Width = 184
    Height = 25
    Caption = 'Show Native Task Dialog'
    TabOrder = 8
    OnClick = ShowDlg
  end
  object btStdMsgDlg: TButton
    Left = 206
    Top = 463
    Width = 184
    Height = 25
    Caption = 'Show Native Message Dialog'
    TabOrder = 9
    OnClick = ShowDlg
  end
  object CBXButton2: TButton
    Left = 537
    Top = 432
    Width = 184
    Height = 25
    Caption = 'Raise Error with Msg Dialog'
    TabOrder = 10
    OnClick = RaiseError
  end
  object ShowTaskDialogCompButton: TButton
    Left = 207
    Top = 418
    Width = 184
    Height = 25
    Caption = 'Show Native Long Messages'
    TabOrder = 11
    OnClick = ShowTaskDialog
  end
  object ShowStyleDialogButton: TButton
    Left = 17
    Top = 418
    Width = 184
    Height = 25
    Caption = 'Show Styled Long Messages'
    TabOrder = 12
    OnClick = ShowStyleDialogButtonClick
  end
  object cbUseStyledDialog: TCheckBox
    Left = 17
    Top = 364
    Width = 161
    Height = 17
    Caption = 'Use modern Styled Dialog'
    Checked = True
    State = cbChecked
    TabOrder = 13
    OnClick = cbUseStyledDialogClick
  end
  object TaskDialog: TTaskDialog
    Buttons = <>
    Caption = 'Caption'
    RadioButtons = <>
    Text = 
      'Message Text Lorem ipsum dolor sit amet, consectetur adipiscing ' +
      'elit, sed do eiusmod tempor incididunt ut labore et dolore magna' +
      ' aliqua. Ut enim ad minim veniam, quis nostrud exercitation ulla' +
      'mco laboris nisi ut aliquip ex ea commodo consequat. Duis aute i' +
      'rure dolor in reprehenderit in voluptate velit esse cillum dolor' +
      'e eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat no' +
      'n proident, sunt in culpa qui officia deserunt mollit anim id es' +
      't laborum.'
    Title = 
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do ' +
      'eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut e' +
      'nim ad minim veniam, quis nostrud exercitation ullamco laboris n' +
      'isi ut aliquip ex ea commodo consequat. Duis aute irure dolor in' +
      ' reprehenderit in voluptate velit esse cillum dolore eu fugiat n' +
      'ulla pariatur. Excepteur sint occaecat cupidatat non proident, s' +
      'unt in culpa qui officia deserunt mollit anim id est laborum'
    Left = 266
    Top = 175
  end
  object StyledTaskDialog: TStyledTaskDialog
    Buttons = <>
    Caption = 'CAPTION'
    RadioButtons = <>
    Text = 
      'Message Text Lorem ipsum dolor sit amet, consectetur adipiscing ' +
      'elit, sed do eiusmod tempor incididunt ut labore et dolore magna' +
      ' aliqua. Ut enim ad minim veniam, quis nostrud exercitation ulla' +
      'mco laboris nisi ut aliquip ex ea commodo consequat. Duis aute i' +
      'rure dolor in reprehenderit in voluptate velit esse cillum dolor' +
      'e eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat no' +
      'n proident, sunt in culpa qui officia deserunt mollit anim id es' +
      't laborum.'
    Title = 
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do ' +
      'eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut e' +
      'nim ad minim veniam, quis nostrud exercitation ullamco laboris n' +
      'isi ut aliquip ex ea commodo consequat. Duis aute irure dolor in' +
      ' reprehenderit in voluptate velit esse cillum dolore eu fugiat n' +
      'ulla pariatur. Excepteur sint occaecat cupidatat non proident, s' +
      'unt in culpa qui officia deserunt mollit anim id est laborum'
    Left = 265
    Top = 231
  end
end
