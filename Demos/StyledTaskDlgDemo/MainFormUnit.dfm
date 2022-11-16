object MainForm: TMainForm
  Left = 0
  Top = 0
  HelpContext = 100
  Caption = 'StyledTaskDialog Demo (c) Ethea S.r.l.'
  ClientHeight = 574
  ClientWidth = 749
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 730
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
  object ExtraGroupBox: TGroupBox
    Left = 0
    Top = 344
    Width = 749
    Height = 230
    Align = alBottom
    Caption = 'Use Style Dialog Component with extra feature'
    TabOrder = 0
    ExplicitTop = 343
    ExplicitWidth = 729
    DesignSize = (
      749
      230)
    object ExpandedTextLabel: TLabel
      Left = 33
      Top = 43
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Expanded Text:'
    end
    object FooterTextLabel: TLabel
      Left = 49
      Top = 105
      Width = 61
      Height = 13
      Alignment = taRightJustify
      Caption = 'Footer Text:'
    end
    object Label1: TLabel
      Left = 28
      Top = 167
      Width = 82
      Height = 13
      Alignment = taRightJustify
      Caption = 'Verification Text:'
    end
    object CaptionLabel: TLabel
      Left = 37
      Top = 21
      Width = 73
      Height = 13
      Alignment = taRightJustify
      Caption = 'Dialog Caption:'
    end
    object StyleLabel: TLabel
      Left = 543
      Top = 186
      Width = 121
      Height = 13
      Anchors = [akRight, akBottom]
      Caption = 'Change application style:'
    end
    object ExpandedTextMemo: TMemo
      Left = 112
      Top = 44
      Width = 425
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Expanded Text'
        'second expanded line')
      TabOrder = 1
      WordWrap = False
    end
    object btUseStyledDialogComp: TButton
      Left = 543
      Top = 123
      Width = 195
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Use StyledTaskDialog comp.'
      TabOrder = 5
      OnClick = UseStyleDialogCompClick
      ExplicitLeft = 523
    end
    object btUseNativeDialogComp: TButton
      Left = 543
      Top = 154
      Width = 195
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Use native TaskDialog comp.'
      TabOrder = 6
      OnClick = UseStyleDialogCompClick
      ExplicitLeft = 523
    end
    object FooterTextMemo: TMemo
      Left = 112
      Top = 106
      Width = 425
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Footer Text'
        'second footer line')
      TabOrder = 2
      WordWrap = False
    end
    object VerificationTextMemo: TMemo
      Left = 112
      Top = 168
      Width = 425
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Verification Text'
        'second Verification line')
      TabOrder = 3
      WordWrap = False
    end
    object rgMainIcon: TRadioGroup
      Left = 543
      Top = 10
      Width = 195
      Height = 107
      Anchors = [akTop, akRight]
      Caption = 'Main Icon'
      ItemIndex = 0
      Items.Strings = (
        'tdiNone'
        'tdiWarning'
        'tdiError'
        'tdiInformation'
        'tdiShield')
      TabOrder = 4
      ExplicitLeft = 523
    end
    object CaptionEdit: TEdit
      Left = 112
      Top = 17
      Width = 425
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Caption'
    end
    object cbChangeStyle: TComboBox
      Left = 543
      Top = 203
      Width = 196
      Height = 21
      Style = csDropDownList
      Anchors = [akRight, akBottom]
      TabOrder = 7
      OnSelect = cbChangeStyleSelect
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 749
    Height = 344
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 729
    ExplicitHeight = 343
    object LeftPanelClient: TPanel
      Left = 1
      Top = 1
      Width = 533
      Height = 342
      Align = alClient
      TabOrder = 0
      ExplicitTop = -4
      ExplicitWidth = 507
      ExplicitHeight = 341
      DesignSize = (
        533
        342)
      object TitleLabel: TLabel
        Left = 7
        Top = 5
        Width = 24
        Height = 13
        Caption = 'Title:'
      end
      object TextMessageLabel: TLabel
        Left = 7
        Top = 52
        Width = 46
        Height = 13
        Caption = 'Message:'
      end
      object FontLabel: TLabel
        Left = 260
        Top = 242
        Width = 71
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Message Font:'
      end
      object edTitle: TEdit
        Left = 7
        Top = 24
        Width = 515
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'Task dialog Tester'
        ExplicitWidth = 495
      end
      object edMessage: TMemo
        Left = 7
        Top = 70
        Width = 515
        Height = 164
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
        WordWrap = False
        ExplicitWidth = 495
        ExplicitHeight = 163
      end
      object cbUseStyledDialog: TCheckBox
        Left = 7
        Top = 241
        Width = 180
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Use modern Styled Dialog'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cbUseStyledDialogClick
      end
      object FontComboBox: TComboBox
        Left = 337
        Top = 239
        Width = 160
        Height = 21
        Anchors = [akLeft, akBottom]
        TabOrder = 3
        Text = 'FontComboBox'
        OnSelect = FontComboBoxSelect
      end
      object btCustomTaskDialog: TButton
        Left = 7
        Top = 266
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Custom Task Dialog'
        TabOrder = 4
        OnClick = ShowDlg
      end
      object btNativeTaskDialog: TButton
        Left = 171
        Top = 266
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Native Task Dialog'
        TabOrder = 5
        OnClick = ShowDlg
      end
      object btRaiseErrorTaskDialog: TButton
        Left = 337
        Top = 266
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Raise Error with Task Dialog'
        TabOrder = 6
        OnClick = RaiseError
      end
      object btCustomMsgDialog: TButton
        Left = 6
        Top = 297
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Custom Message Dialog'
        TabOrder = 7
        OnClick = ShowDlg
      end
      object btNativeMsgDialog: TButton
        Left = 171
        Top = 297
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Native Message Dialog'
        TabOrder = 8
        OnClick = ShowDlg
      end
      object btRaiseErrorMsgDialog: TButton
        Left = 337
        Top = 297
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Raise Error with Msg Dialog'
        TabOrder = 9
        OnClick = RaiseError
      end
    end
    object RightPanel: TPanel
      Left = 534
      Top = 1
      Width = 214
      Height = 342
      Align = alRight
      TabOrder = 1
      ExplicitLeft = 514
      ExplicitHeight = 341
      DesignSize = (
        214
        342)
      object DefaultButtonLabel: TLabel
        Left = 10
        Top = 317
        Width = 74
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Default Button:'
      end
      object rgDlgType: TRadioGroup
        Left = 9
        Top = 4
        Width = 196
        Height = 92
        Caption = 'Dialog Type'
        TabOrder = 0
      end
      object clbButtons: TCheckListBox
        Left = 9
        Top = 100
        Width = 196
        Height = 208
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 1
      end
      object DefaultButtonComboBox: TComboBox
        Left = 90
        Top = 314
        Width = 115
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        DropDownCount = 12
        TabOrder = 2
      end
    end
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
    Left = 178
    Top = 119
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
    Left = 297
    Top = 119
  end
end
