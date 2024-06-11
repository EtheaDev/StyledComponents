object MainForm: TMainForm
  Left = 0
  Top = 0
  HelpContext = 100
  ActiveControl = CaptionEdit
  Caption = 'StyledTaskDialog Demo (c) Ethea S.r.l.'
  ClientHeight = 573
  ClientWidth = 745
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 730
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object ExtraGroupBox: TGroupBox
    Left = 0
    Top = 343
    Width = 745
    Height = 230
    Align = alBottom
    Caption = 'Use Style Dialog Component with extra feature'
    TabOrder = 0
    DesignSize = (
      745
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
      Left = 50
      Top = 105
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = 'Footer Text:'
    end
    object Label1: TLabel
      Left = 26
      Top = 167
      Width = 84
      Height = 13
      Alignment = taRightJustify
      Caption = 'Verification Text:'
    end
    object CaptionLabel: TLabel
      Left = 29
      Top = 21
      Width = 81
      Height = 13
      Alignment = taRightJustify
      Caption = 'Dialog Caption:'
    end
    object StyleLabel: TLabel
      Left = 523
      Top = 186
      Width = 130
      Height = 13
      Anchors = [akRight, akBottom]
      Caption = 'Change application style:'
      ExplicitLeft = 531
    end
    object ExpandedTextMemo: TMemo
      Left = 112
      Top = 44
      Width = 405
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Expanded Text'
        'second expanded line')
      TabOrder = 1
      WordWrap = False
    end
    object btUseStyledDialogComp: TButton
      Left = 523
      Top = 124
      Width = 195
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Use StyledTaskDialog comp.'
      TabOrder = 5
      OnClick = UseStyleDialogCompClick
    end
    object btUseNativeDialogComp: TButton
      Left = 523
      Top = 155
      Width = 195
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Use native TaskDialog comp.'
      TabOrder = 6
      OnClick = UseStyleDialogCompClick
    end
    object FooterTextMemo: TMemo
      Left = 112
      Top = 106
      Width = 405
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
      Width = 405
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Verification Text'
        'second Verification line')
      TabOrder = 3
      WordWrap = False
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
      Top = 17
      Width = 405
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Caption'
    end
    object cbChangeStyle: TComboBox
      Left = 523
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
    Width = 745
    Height = 343
    Align = alClient
    TabOrder = 1
    object LeftPanelClient: TPanel
      Left = 1
      Top = 1
      Width = 529
      Height = 341
      Align = alClient
      TabOrder = 0
      DesignSize = (
        529
        341)
      object TitleLabel: TLabel
        Left = 7
        Top = 5
        Width = 25
        Height = 13
        Caption = 'Title:'
      end
      object TextMessageLabel: TLabel
        Left = 7
        Top = 52
        Width = 48
        Height = 13
        Caption = 'Message:'
      end
      object FontLabel: TLabel
        Left = 256
        Top = 238
        Width = 75
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Message Font:'
        ExplicitTop = 239
      end
      object Label2: TLabel
        Left = 16
        Top = 240
        Width = 89
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Dlg Buttons type:'
        ExplicitTop = 241
      end
      object AlphaLabel: TLabel
        Left = 358
        Top = 5
        Width = 33
        Height = 13
        Caption = 'Alpha:'
      end
      object edTitle: TEdit
        Left = 7
        Top = 24
        Width = 345
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'Task dialog Tester'
      end
      object edMessage: TMemo
        Left = 6
        Top = 70
        Width = 495
        Height = 162
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
      object FontComboBox: TComboBox
        Left = 337
        Top = 237
        Width = 160
        Height = 21
        Anchors = [akLeft, akBottom]
        TabOrder = 3
        Text = 'FontComboBox'
        OnSelect = FontComboBoxSelect
      end
      object btCustomTaskDialog: TButton
        Left = 7
        Top = 264
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Custom Task Dialog'
        TabOrder = 4
        OnClick = ShowDlg
      end
      object btNativeTaskDialog: TButton
        Left = 171
        Top = 264
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Native Task Dialog'
        TabOrder = 5
        OnClick = ShowDlg
      end
      object btRaiseErrorTaskDialog: TButton
        Left = 337
        Top = 264
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Raise Database Error'
        TabOrder = 6
        OnClick = RaiseDatabaseError
      end
      object btCustomMsgDialog: TButton
        Left = 6
        Top = 295
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Custom Message Dialog'
        TabOrder = 7
        OnClick = ShowDlg
      end
      object btNativeMsgDialog: TButton
        Left = 171
        Top = 295
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Native Message Dialog'
        TabOrder = 8
        OnClick = ShowDlg
      end
      object btRaiseErrorMsgDialog: TButton
        Left = 337
        Top = 295
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Raise Generic Error'
        TabOrder = 9
        OnClick = RaiseError
      end
      object FamilyComboBox: TComboBox
        Left = 111
        Top = 237
        Width = 130
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemIndex = 0
        TabOrder = 10
        Text = 'Classic'
        OnSelect = FamilyComboBoxSelect
        Items.Strings = (
          'Classic'
          'Bootstrap'
          'Angular-Light'
          'Angular-Dark'
          'Basic-Colors'
          'SVG-Colors')
      end
      object gbResult: TGroupBox
        Left = 410
        Top = 4
        Width = 92
        Height = 60
        Anchors = [akTop, akRight]
        Caption = 'ModalResult'
        TabOrder = 11
        object MRLabel: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 25
          Width = 82
          Height = 30
          Margins.Top = 10
          Align = alClient
          Alignment = taCenter
          ExplicitWidth = 3
          ExplicitHeight = 13
        end
      end
      object AlphaBlendSpinEdit: TSpinEdit
        Left = 358
        Top = 24
        Width = 46
        Height = 22
        Increment = 10
        MaxValue = 255
        MinValue = 100
        TabOrder = 1
        Value = 255
        OnChange = AlphaBlendSpinEditChange
      end
    end
    object RightPanel: TPanel
      Left = 530
      Top = 1
      Width = 214
      Height = 341
      Align = alRight
      TabOrder = 1
      DesignSize = (
        214
        341)
      object DefaultButtonLabel: TLabel
        Left = 5
        Top = 316
        Width = 79
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Default Button:'
        ExplicitTop = 317
      end
      object rgDlgType: TRadioGroup
        Left = 6
        Top = 4
        Width = 196
        Height = 102
        Caption = 'Dialog Type'
        TabOrder = 0
      end
      object clbButtons: TCheckListBox
        Left = 9
        Top = 112
        Width = 196
        Height = 195
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 1
      end
      object DefaultButtonComboBox: TComboBox
        Left = 90
        Top = 313
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
    Buttons = <
      item
        Caption = 'Custom No'
        CommandLinkHint = 'Custom No to All'
        ModalResult = 13
      end
      item
        Caption = 'custom Yes'
        CommandLinkHint = 'Custom Yes To All'
        ElevationRequired = True
        ModalResult = 14
      end>
    Caption = 'Caption'
    Flags = [tfEnableHyperlinks, tfAllowDialogCancellation, tfUseCommandLinks, tfPositionRelativeToWindow]
    FooterIcon = 1
    FooterText = 
      'The file was created: <A HREF="C:\Windows\System32\license.rtf">' +
      'license.rtf</A>'
    HelpContext = 100
    RadioButtons = <
      item
        Caption = 'Radio Button 1'
      end
      item
        Caption = 'Radio Button 2'
        Default = True
      end>
    Text = 
      'Message Text Lorem ipsum dolor sit amet, consectetur adipiscing ' +
      'elit, sed do eiusmod tempor incididunt ut labore et dolore magna' +
      ' aliqua. Ut enim ad minim veniam, quis nostrud exercitation ulla' +
      'mco laboris nisi ut aliquip ex ea commodo consequat. Duis aute i' +
      'rure dolor in reprehenderit in voluptate velit esse cillum dolor' +
      'e eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat no' +
      'n proident, sunt in culpa qui officia deserunt mollit anim id es' +
      't laborum.'
    Title = 'Task Dialog Tester'
    VerificationText = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit'
    OnButtonClicked = TaskDialogButtonClicked
    OnDialogConstructed = TaskDialogDialogConstructed
    OnDialogCreated = TaskDialogDialogCreated
    OnDialogDestroyed = TaskDialogDialogDestroyed
    OnExpanded = TaskDialogExpanded
    OnHyperlinkClicked = TaskDialogHyperlinkClicked
    OnNavigated = TaskDialogNavigated
    OnRadioButtonClicked = TaskDialogRadioButtonClicked
    OnTimer = TaskDialogTimer
    OnVerificationClicked = TaskDialogVerificationClicked
    Left = 178
    Top = 119
  end
  object StyledTaskDialog: TStyledTaskDialog
    Buttons = <
      item
        Caption = 'Custom No'
        CommandLinkHint = 'Custom No to All'
        ModalResult = 13
      end
      item
        Caption = 'Custom Yes'
        CommandLinkHint = 'Custom Yes To All'
        ElevationRequired = True
        ModalResult = 14
      end>
    Caption = 'CAPTION OF DIALOG'
    ExpandedText = 'Expanded Text'
    Flags = [tfAllowDialogCancellation, tfUseCommandLinks, tfPositionRelativeToWindow]
    FooterIcon = 1
    FooterText = 
      'The file was created: <A HREF="C:\Windows\System32\license.rtf">' +
      'license.rtf</A>'
    HelpContext = 100
    RadioButtons = <
      item
        Caption = 'Radio Button 1'
      end
      item
        Caption = 'Radio Button 2'
        Default = True
      end>
    Text = 
      'Message Text Lorem ipsum dolor sit amet, consectetur adipiscing ' +
      'elit, sed do eiusmod tempor incididunt ut labore et dolore magna' +
      ' aliqua. Ut enim ad minim veniam, quis nostrud exercitation ulla' +
      'mco laboris nisi ut aliquip ex ea commodo consequat. Duis aute i' +
      'rure dolor in reprehenderit in voluptate velit esse cillum dolor' +
      'e eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat no' +
      'n proident, sunt in culpa qui officia deserunt mollit anim id es' +
      't laborum.'
    Title = 'Task Dialog Tester'
    VerificationText = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit'
    OnButtonClicked = TaskDialogButtonClicked
    OnDialogConstructed = TaskDialogDialogConstructed
    OnDialogCreated = TaskDialogDialogCreated
    OnDialogDestroyed = TaskDialogDialogDestroyed
    OnExpanded = TaskDialogExpanded
    OnHyperlinkClicked = TaskDialogHyperlinkClicked
    OnNavigated = TaskDialogNavigated
    OnRadioButtonClicked = TaskDialogRadioButtonClicked
    OnTimer = TaskDialogTimer
    OnVerificationClicked = TaskDialogVerificationClicked
    Left = 297
    Top = 119
  end
end
