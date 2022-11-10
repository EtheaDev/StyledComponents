object MainForm: TMainForm
  Left = 0
  Top = 0
  HelpContext = 100
  Caption = 'StyledTaskDialog Demo (c) Ethea S.r.l.'
  ClientHeight = 660
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
    Left = 512
    Top = 353
    Width = 74
    Height = 13
    Alignment = taRightJustify
    Caption = 'Default Button:'
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
    Top = 51
    Width = 26
    Height = 13
    Caption = 'Text:'
  end
  object FontLabel: TLabel
    Left = 248
    Top = 348
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = 'Font:'
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
    Left = 10
    Top = 64
    Width = 434
    Height = 275
    TabOrder = 2
    WordWrap = False
  end
  object btCustomTaskDialog: TButton
    Left = 7
    Top = 370
    Width = 210
    Height = 25
    Caption = 'Show Custom Task Dialog'
    TabOrder = 6
    OnClick = ShowDlg
  end
  object rgDlgType: TRadioGroup
    Left = 448
    Top = 16
    Width = 273
    Height = 105
    Caption = 'Dialog Type'
    TabOrder = 1
  end
  object clbButtons: TCheckListBox
    Left = 450
    Top = 127
    Width = 273
    Height = 212
    ItemHeight = 13
    TabOrder = 3
  end
  object btCustomMsgDialog: TButton
    Left = 8
    Top = 401
    Width = 210
    Height = 25
    Caption = 'Show Custom Message Dialog'
    TabOrder = 8
    OnClick = ShowDlg
  end
  object btRaiseErrorTaskDialog: TButton
    Left = 513
    Top = 372
    Width = 210
    Height = 25
    Caption = 'Raise Error with Task Dialog'
    TabOrder = 10
    OnClick = RaiseError
  end
  object DefaultButtonComboBox: TComboBox
    Left = 592
    Top = 345
    Width = 131
    Height = 21
    Style = csDropDownList
    DropDownCount = 12
    TabOrder = 5
  end
  object btNativeTaskDialog: TButton
    Left = 234
    Top = 370
    Width = 210
    Height = 25
    Caption = 'Show Native Task Dialog'
    TabOrder = 7
    OnClick = ShowDlg
  end
  object btNativeMsgDialog: TButton
    Left = 234
    Top = 401
    Width = 210
    Height = 25
    Caption = 'Show Native Message Dialog'
    TabOrder = 9
    OnClick = ShowDlg
  end
  object btRaiseErrorMsgDialog: TButton
    Left = 513
    Top = 401
    Width = 210
    Height = 25
    Caption = 'Raise Error with Msg Dialog'
    TabOrder = 11
    OnClick = RaiseError
  end
  object cbUseStyledDialog: TCheckBox
    Left = 8
    Top = 347
    Width = 137
    Height = 17
    Caption = 'Use modern Styled Dialog'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = cbUseStyledDialogClick
  end
  object ExtraGroupBox: TGroupBox
    Left = 0
    Top = 432
    Width = 731
    Height = 228
    Align = alBottom
    Caption = 'Use Style Dialog Component with extra feature'
    TabOrder = 12
    ExplicitLeft = 7
    object ExpandedTextLabel: TLabel
      Left = 33
      Top = 41
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Expanded Text:'
    end
    object FooterTextLabel: TLabel
      Left = 49
      Top = 103
      Width = 61
      Height = 13
      Alignment = taRightJustify
      Caption = 'Footer Text:'
    end
    object Label1: TLabel
      Left = 28
      Top = 165
      Width = 82
      Height = 13
      Alignment = taRightJustify
      Caption = 'Verification Text:'
    end
    object CaptionLabel: TLabel
      Left = 37
      Top = 19
      Width = 73
      Height = 13
      Alignment = taRightJustify
      Caption = 'Dialog Caption:'
    end
    object ExpandedTextMemo: TMemo
      Left = 112
      Top = 42
      Width = 411
      Height = 56
      Lines.Strings = (
        'Expanded Text'
        'second expanded line')
      TabOrder = 1
      WordWrap = False
    end
    object btUseStyledDialogComp: TButton
      Left = 526
      Top = 14
      Width = 195
      Height = 25
      Caption = 'Use StyledTaskDialog comp.'
      TabOrder = 4
      OnClick = UseStyleDialogCompClick
    end
    object btUseNativeDialogComp: TButton
      Left = 526
      Top = 45
      Width = 195
      Height = 25
      Caption = 'Use native TaskDialog comp.'
      TabOrder = 5
      OnClick = UseStyleDialogCompClick
    end
    object FooterTextMemo: TMemo
      Left = 112
      Top = 104
      Width = 411
      Height = 56
      Lines.Strings = (
        'Footer Text'
        'second footer line')
      TabOrder = 2
      WordWrap = False
    end
    object VerificationTextMemo: TMemo
      Left = 112
      Top = 166
      Width = 411
      Height = 56
      Lines.Strings = (
        'Verification Text'
        'second Verification line')
      TabOrder = 3
      WordWrap = False
    end
    object rgMainIcon: TRadioGroup
      Left = 538
      Top = 76
      Width = 190
      Height = 121
      Caption = 'Main Icon'
      ItemIndex = 0
      Items.Strings = (
        'tdiNone'
        'tdiWarning'
        'tdiError'
        'tdiInformation'
        'tdiShield')
      TabOrder = 6
    end
    object CaptionEdit: TEdit
      Left = 112
      Top = 15
      Width = 411
      Height = 21
      TabOrder = 0
      Text = 'Caption'
    end
  end
  object FontComboBox: TComboBox
    Left = 280
    Top = 345
    Width = 164
    Height = 21
    TabOrder = 13
    Text = 'FontComboBox'
    OnSelect = FontComboBoxSelect
  end
  object TaskDialog: TTaskDialog
    Buttons = <>
    Caption = 'Caption'
    ExpandedText = 'Expanded Text'
    Flags = [tfEnableHyperlinks, tfAllowDialogCancellation]
    FooterText = 'Footer Text'
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
    VerificationText = 'Verification Test'
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
