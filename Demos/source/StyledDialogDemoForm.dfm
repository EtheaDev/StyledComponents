object fmStyledTaskDialog: TfmStyledTaskDialog
  Left = 0
  Top = 0
  HelpContext = 100
  ActiveControl = CaptionEdit
  Caption = 'StyledTaskDialog Demo (c) Ethea S.r.l.'
  ClientHeight = 573
  ClientWidth = 727
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
    Width = 727
    Height = 230
    Align = alBottom
    Caption = 'Use Style Dialog Component with extra feature'
    TabOrder = 0
    DesignSize = (
      727
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
    object VerificationTextLabel: TLabel
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
    object ExpandedTextMemo: TMemo
      Left = 112
      Top = 44
      Width = 387
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Expanded Text'
        'second expanded line')
      TabOrder = 1
      WordWrap = False
    end
    object btUseStyledDialogComp: TButton
      Left = 521
      Top = 160
      Width = 195
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Use StyledTaskDialog comp.'
      TabOrder = 5
      OnClick = UseStyleDialogCompClick
    end
    object btUseNativeDialogComp: TButton
      Left = 521
      Top = 191
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
      Width = 387
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
      Width = 384
      Height = 48
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Verification Text'
        'second Verification line')
      TabOrder = 3
      WordWrap = False
    end
    object rgMainIcon: TRadioGroup
      Left = 521
      Top = 10
      Width = 195
      Height = 142
      Anchors = [akTop, akRight]
      Caption = 'Main Icon'
      ItemIndex = 3
      Items.Strings = (
        'tdiNone'
        'tdiWarning'
        'tdiError'
        'tdiInformation'
        'tdiShield'
        'tdiQuestion')
      TabOrder = 4
    end
    object CaptionEdit: TEdit
      Left = 112
      Top = 17
      Width = 387
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Caption'
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 727
    Height = 343
    Align = alClient
    TabOrder = 1
    object LeftPanelClient: TPanel
      Left = 1
      Top = 1
      Width = 511
      Height = 341
      Align = alClient
      TabOrder = 0
      DesignSize = (
        511
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
      object DlgButtonTypeLabel: TLabel
        Left = 16
        Top = 186
        Width = 89
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Dlg Buttons type:'
      end
      object AlphaLabel: TLabel
        Left = 367
        Top = 7
        Width = 33
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Alpha:'
      end
      object ButtonsWidthLabel: TLabel
        Left = 299
        Top = 7
        Width = 60
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Btns Width:'
      end
      object lbAutoClickDelay: TLabel
        Left = 419
        Top = 284
        Width = 54
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Delay (ms):'
      end
      object edTitle: TEdit
        Left = 7
        Top = 24
        Width = 286
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'Task dialog Tester'
      end
      object edMessage: TMemo
        Left = 5
        Top = 70
        Width = 492
        Height = 107
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
      end
      object btStyledTaskDialog: TButton
        Left = 173
        Top = 210
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Styled Task Dialog'
        TabOrder = 7
        OnClick = ShowDlg
      end
      object btNativeTaskDialog: TButton
        Left = 7
        Top = 210
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Native Task Dialog'
        TabOrder = 6
        OnClick = ShowDlg
      end
      object btRaiseDatabaseError: TButton
        Left = 7
        Top = 303
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Raise Database Error'
        TabOrder = 14
        OnClick = RaiseDatabaseError
      end
      object btStyledMsgDialog: TButton
        Left = 173
        Top = 241
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Styled Message Dialog'
        TabOrder = 10
        OnClick = ShowDlg
      end
      object btNativeMsgDialog: TButton
        Left = 7
        Top = 241
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Native Message Dialog'
        TabOrder = 9
        OnClick = ShowDlg
      end
      object btRaiseGenericError: TButton
        Left = 173
        Top = 303
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Raise Generic Error'
        TabOrder = 15
        OnClick = RaiseError
      end
      object FamilyComboBox: TComboBox
        Left = 111
        Top = 183
        Width = 130
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemIndex = 0
        TabOrder = 5
        Text = 'Classic'
        OnSelect = InitializeDialogsClick
        Items.Strings = (
          'Classic'
          'Bootstrap'
          'Angular-Light'
          'Angular-Dark'
          'Basic-Colors'
          'SVG-Colors')
      end
      object gbResult: TGroupBox
        Left = 419
        Top = 4
        Width = 79
        Height = 60
        Anchors = [akTop, akRight]
        Caption = 'ModalResult'
        TabOrder = 3
        object MRLabel: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 25
          Width = 69
          Height = 30
          Margins.Top = 10
          Align = alClient
          Alignment = taCenter
          AutoSize = False
        end
      end
      object AlphaBlendSpinEdit: TSpinEdit
        Left = 367
        Top = 24
        Width = 46
        Height = 22
        Anchors = [akTop, akRight]
        Increment = 10
        MaxValue = 255
        MinValue = 100
        TabOrder = 2
        Value = 255
        OnChange = InitializeDialogsClick
      end
      object cbUseCommandLinks: TCheckBox
        Left = 339
        Top = 210
        Width = 170
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Use Command Links'
        TabOrder = 8
        OnClick = InitializeDialogsClick
      end
      object cbUseTitleInMessageDlg: TCheckBox
        Left = 339
        Top = 245
        Width = 170
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Add Title in MessageDlg'
        TabOrder = 11
        OnClick = InitializeDialogsClick
      end
      object btNativeShowMessage: TButton
        Left = 7
        Top = 272
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Native ShowMessage'
        TabOrder = 12
        OnClick = ShowDlg
      end
      object btStyledShowMessage: TButton
        Left = 173
        Top = 272
        Width = 160
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Styled ShowMessage'
        TabOrder = 13
        OnClick = ShowDlg
      end
      object ButtonsWidthSpinEdit: TSpinEdit
        Left = 299
        Top = 24
        Width = 62
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 150
        MinValue = 60
        TabOrder = 1
        Value = 74
      end
      object AutoClickDelaySpinEdit: TSpinEdit
        Left = 419
        Top = 303
        Width = 62
        Height = 22
        Anchors = [akLeft, akBottom]
        Increment = 1000
        MaxValue = 100000
        MinValue = 1000
        TabOrder = 17
        Value = 5000
        OnChange = InitializeDialogsClick
      end
      object cbAutoClick: TCheckBox
        Left = 339
        Top = 305
        Width = 74
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'AutoClick'
        TabOrder = 16
        OnClick = InitializeDialogsClick
      end
    end
    object RightPanel: TPanel
      Left = 512
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
        ItemHeight = 17
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
    ExpandedText = 
      'The file was created: <A HREF="C:\Windows\System32\license.rtf">' +
      'license.rtf</A> Expanded Text Expanded Text Expanded Text Expand' +
      'ed Text Expanded Text Expanded Text Expanded Text Expanded Text ' +
      'Expanded Text Expanded Text Expanded Text '
    Flags = [tfEnableHyperlinks, tfAllowDialogCancellation, tfUseCommandLinks, tfExpandedByDefault, tfPositionRelativeToWindow]
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
    ExpandedText = 
      'The file was created: <A HREF="C:\Windows\System32\license.rtf">' +
      'license.rtf</A> Expanded Text Expanded Text Expanded Text Expand' +
      'ed Text Expanded Text Expanded Text Expanded Text Expanded Text ' +
      'Expanded Text Expanded Text Expanded Text '
    Flags = [tfAllowDialogCancellation, tfUseCommandLinks, tfExpandedByDefault, tfPositionRelativeToWindow]
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
    ButtonsWidth = 100
    Left = 297
    Top = 119
  end
end
