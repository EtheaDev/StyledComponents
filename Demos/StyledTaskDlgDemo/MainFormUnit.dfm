object MainForm: TMainForm
  Left = 0
  Top = 0
  HelpContext = 100
  Caption = 'StyledTaskDialog Demo (c) Ethea S.r.l.'
  ClientHeight = 573
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object ExtraGroupBox: TGroupBox
    Left = 0
    Top = 343
    Width = 729
    Height = 230
    Align = alBottom
    Caption = 'Use Style Dialog Component with extra feature'
    TabOrder = 0
    ExplicitTop = 572
    ExplicitWidth = 727
    DesignSize = (
      729
      230)
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
    object StyleLabel: TLabel
      Left = 523
      Top = 184
      Width = 121
      Height = 13
      Caption = 'Change application style:'
    end
    object ExpandedTextMemo: TMemo
      Left = 112
      Top = 42
      Width = 405
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Expanded Text'
        'second expanded line')
      TabOrder = 1
      WordWrap = False
      ExplicitWidth = 403
    end
    object btUseStyledDialogComp: TButton
      Left = 523
      Top = 123
      Width = 195
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Use StyledTaskDialog comp.'
      TabOrder = 5
      OnClick = UseStyleDialogCompClick
    end
    object btUseNativeDialogComp: TButton
      Left = 523
      Top = 154
      Width = 195
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Use native TaskDialog comp.'
      TabOrder = 6
      OnClick = UseStyleDialogCompClick
    end
    object FooterTextMemo: TMemo
      Left = 112
      Top = 104
      Width = 405
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Footer Text'
        'second footer line')
      TabOrder = 2
      WordWrap = False
      ExplicitWidth = 403
    end
    object VerificationTextMemo: TMemo
      Left = 112
      Top = 166
      Width = 405
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Verification Text'
        'second Verification line')
      TabOrder = 3
      WordWrap = False
      ExplicitWidth = 403
    end
    object rgMainIcon: TRadioGroup
      Left = 523
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
    end
    object CaptionEdit: TEdit
      Left = 112
      Top = 15
      Width = 405
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Caption'
      ExplicitWidth = 403
    end
    object cbChangeStyle: TComboBox
      Left = 523
      Top = 201
      Width = 196
      Height = 21
      Style = csDropDownList
      TabOrder = 7
      OnSelect = cbChangeStyleSelect
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 729
    Height = 343
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 731
    ExplicitHeight = 395
    DesignSize = (
      729
      343)
    object TitleLabel: TLabel
      Left = 4
      Top = 5
      Width = 24
      Height = 13
      Caption = 'Title:'
    end
    object TextMessageLabel: TLabel
      Left = 4
      Top = 52
      Width = 46
      Height = 13
      Caption = 'Message:'
    end
    object edTitle: TEdit
      Left = 4
      Top = 24
      Width = 504
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Task dialog Tester'
      ExplicitWidth = 544
    end
    object edMessage: TMemo
      Left = 4
      Top = 70
      Width = 504
      Height = 163
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 1
      WordWrap = False
      ExplicitWidth = 544
      ExplicitHeight = 232
    end
    object RightPanel: TPanel
      Left = 514
      Top = 1
      Width = 214
      Height = 341
      Align = alRight
      TabOrder = 3
      ExplicitLeft = 554
      ExplicitHeight = 410
      DesignSize = (
        214
        341)
      object DefaultButtonLabel: TLabel
        Left = 10
        Top = 297
        Width = 74
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Default Button:'
        ExplicitTop = 408
      end
      object rgDlgType: TRadioGroup
        Left = 9
        Top = 4
        Width = 197
        Height = 88
        Caption = 'Dialog Type'
        TabOrder = 0
      end
      object clbButtons: TCheckListBox
        Left = 9
        Top = 98
        Width = 197
        Height = 195
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 1
      end
      object DefaultButtonComboBox: TComboBox
        Left = 9
        Top = 313
        Width = 189
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        DropDownCount = 12
        TabOrder = 2
        ExplicitTop = 424
      end
    end
    object PanelCenter: TPanel
      Left = 4
      Top = 239
      Width = 505
      Height = 98
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      ExplicitTop = 308
      DesignSize = (
        505
        98)
      object FontLabel: TLabel
        Left = 262
        Top = 9
        Width = 71
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Message Font:'
      end
      object cbUseStyledDialog: TCheckBox
        Left = 6
        Top = 8
        Width = 180
        Height = 17
        Caption = 'Use modern Styled Dialog'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbUseStyledDialogClick
      end
      object btCustomTaskDialog: TButton
        Left = 7
        Top = 33
        Width = 160
        Height = 25
        Caption = 'Custom Task Dialog'
        TabOrder = 2
        OnClick = ShowDlg
      end
      object btNativeTaskDialog: TButton
        Left = 173
        Top = 33
        Width = 160
        Height = 25
        Caption = 'Native Task Dialog'
        TabOrder = 3
        OnClick = ShowDlg
      end
      object btRaiseErrorTaskDialog: TButton
        Left = 339
        Top = 33
        Width = 160
        Height = 25
        Caption = 'Raise Error with Task Dialog'
        TabOrder = 4
        OnClick = RaiseError
      end
      object btCustomMsgDialog: TButton
        Left = 6
        Top = 64
        Width = 160
        Height = 25
        Caption = 'Custom Message Dialog'
        TabOrder = 5
        OnClick = ShowDlg
      end
      object btNativeMsgDialog: TButton
        Left = 173
        Top = 64
        Width = 160
        Height = 25
        Caption = 'Native Message Dialog'
        TabOrder = 6
        OnClick = ShowDlg
      end
      object btRaiseErrorMsgDialog: TButton
        Left = 339
        Top = 64
        Width = 160
        Height = 25
        Caption = 'Raise Error with Msg Dialog'
        TabOrder = 7
        OnClick = RaiseError
      end
      object FontComboBox: TComboBox
        Left = 339
        Top = 6
        Width = 160
        Height = 21
        Anchors = [akLeft, akBottom]
        TabOrder = 1
        Text = 'FontComboBox'
        OnSelect = FontComboBoxSelect
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
