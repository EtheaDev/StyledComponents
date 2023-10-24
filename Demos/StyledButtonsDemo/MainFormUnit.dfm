object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = ShowEditButton
  Caption = 'Styled Buttons Demo'
  ClientHeight = 691
  ClientWidth = 895
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnBeforeMonitorDpiChanged = FormBeforeMonitorDpiChanged
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 895
    Height = 70
    Align = alTop
    ParentColor = True
    TabOrder = 0
    object StyleLabel: TLabel
      Left = 348
      Top = 8
      Width = 130
      Height = 13
      Caption = 'Change application style:'
    end
    object ShowEditButton: TStyledButton
      Left = 10
      Top = 8
      Width = 172
      Height = 50
      Action = TestAction
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      PopUpMenu = PopupMenu
      ParentFont = False
      Caption = '&Show Editor'
      Images = VirtualImageList32
      TabOrder = 0
    end
    object StyledButtonSquare: TStyledButton
      Left = 203
      Top = 8
      Width = 54
      Height = 50
      ImageAlignment = iaCenter
      Images = VirtualImageList32
      ImageIndex = 11
      StyleFamily = 'Bootstrap'
      StyleClass = 'Danger'
      TabOrder = 1
    end
    object StyledButtonCircular: TStyledButton
      Left = 276
      Top = 8
      Width = 50
      Height = 50
      ImageAlignment = iaCenter
      Images = VirtualImageList32
      ImageIndex = 18
      StyleDrawType = btEllipse
      StyleFamily = 'Bootstrap'
      StyleClass = 'Warning'
      StyleAppearance = 'Outline'
      TabOrder = 2
    end
    object DefaultStyledButton: TStyledButton
      Left = 599
      Top = 8
      Width = 94
      Height = 50
      OnClick = ButtonClick
      PopUpMenu = PopupMenu
      Caption = 'Default'
      Default = True
      TabOrder = 4
    end
    object CancelStyledButton: TStyledButton
      Left = 712
      Top = 8
      Width = 94
      Height = 50
      OnClick = ButtonClick
      PopUpMenu = PopupMenu
      Caption = 'Cancel'
      Cancel = True
      TabOrder = 5
    end
    object cbChangeStyle: TComboBox
      Left = 348
      Top = 27
      Width = 190
      Height = 21
      Style = csDropDownList
      DropDownCount = 12
      TabOrder = 3
      OnSelect = cbChangeStyleSelect
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 70
    Width = 895
    Height = 621
    ActivePage = tsBootstrap
    Align = alClient
    Images = VirtualImageList32
    TabOrder = 1
    object tsBootstrap: TTabSheet
      Caption = 'Bootstrap Buttons'
      ImageIndex = 20
      object BootStrapLinkLabel: TLinkLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 352
        Height = 17
        Caption = 
          'Similar to: <A HREF="https://getbootstrap.com/docs/4.0/component' +
          's/buttons/">https://getbootstrap.com/docs/4.0/components/buttons' +
          '/</A>'
        TabOrder = 0
        OnLinkClick = LinkLabelLinkClick
      end
      object gbBootstrapNormal: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Normal Buttons'
        TabOrder = 1
        object btn_Primary: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Bootstrap'
          TabOrder = 0
        end
        object btn_Secondary: TStyledButton
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Secondary'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Secondary'
          TabOrder = 1
        end
        object btn_Success: TStyledButton
          AlignWithMargins = True
          Left = 181
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Success'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Success'
          TabOrder = 2
        end
        object btn_Danger: TStyledButton
          AlignWithMargins = True
          Left = 279
          Top = 18
          Width = 75
          Height = 36
          Align = alLeft
          Caption = 'Danger'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Danger'
          TabOrder = 3
        end
        object btn_Warning: TStyledButton
          AlignWithMargins = True
          Left = 360
          Top = 18
          Width = 81
          Height = 36
          Align = alLeft
          Caption = 'Warning'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Warning'
          TabOrder = 4
        end
        object btn_Info: TStyledButton
          AlignWithMargins = True
          Left = 447
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Info'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Info'
          TabOrder = 5
        end
        object btn_Light: TStyledButton
          AlignWithMargins = True
          Left = 512
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Light'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Light'
          TabOrder = 6
        end
        object btn_Dark: TStyledButton
          AlignWithMargins = True
          Left = 577
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Dark'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Dark'
          TabOrder = 7
        end
      end
      object gbBootstrapOutlined: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 68
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Outlined Buttons'
        TabOrder = 2
        object btn_OutlinePrimary: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Bootstrap'
          StyleAppearance = 'Outline'
          TabOrder = 0
        end
        object btn_OutlineSecondary: TStyledButton
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Secondary'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Secondary'
          StyleAppearance = 'Outline'
          TabOrder = 1
        end
        object btn_OutlineSuccess: TStyledButton
          AlignWithMargins = True
          Left = 181
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Success'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Success'
          StyleAppearance = 'Outline'
          TabOrder = 2
        end
        object btn_OutlineDanger: TStyledButton
          AlignWithMargins = True
          Left = 279
          Top = 18
          Width = 75
          Height = 36
          Align = alLeft
          Caption = 'Danger'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Danger'
          StyleAppearance = 'Outline'
          TabOrder = 3
        end
        object btn_OutlineWarning: TStyledButton
          AlignWithMargins = True
          Left = 360
          Top = 18
          Width = 81
          Height = 36
          Align = alLeft
          Caption = 'Warning'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Warning'
          StyleAppearance = 'Outline'
          TabOrder = 4
        end
        object btn_OutlineInfo: TStyledButton
          AlignWithMargins = True
          Left = 447
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Info'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Info'
          StyleAppearance = 'Outline'
          TabOrder = 5
        end
        object btn_OutlineLight: TStyledButton
          AlignWithMargins = True
          Left = 512
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Light'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Light'
          StyleAppearance = 'Outline'
          TabOrder = 6
        end
        object btn_OutlineDark: TStyledButton
          AlignWithMargins = True
          Left = 577
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Dark'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Dark'
          StyleAppearance = 'Outline'
          TabOrder = 7
        end
      end
      object gbBuutstrapDisabled: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 133
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Disabled Buttons'
        TabOrder = 3
        object btn_DisabledPrimary: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Primary'
          StyleFamily = 'Bootstrap'
          TabOrder = 0
        end
        object btn_DisabledSecondary: TStyledButton
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Secondary'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Secondary'
          TabOrder = 1
        end
        object btn_DisabledSuccess: TStyledButton
          AlignWithMargins = True
          Left = 181
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Success'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Success'
          TabOrder = 2
        end
        object btn_DisabledDanger: TStyledButton
          AlignWithMargins = True
          Left = 279
          Top = 18
          Width = 75
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Danger'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Danger'
          TabOrder = 3
        end
        object btn_DisabledWarning: TStyledButton
          AlignWithMargins = True
          Left = 360
          Top = 18
          Width = 81
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Warning'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Warning'
          TabOrder = 4
        end
        object btn_DisabledInfo: TStyledButton
          AlignWithMargins = True
          Left = 447
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Info'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Info'
          TabOrder = 5
        end
        object btn_DisabledLight: TStyledButton
          AlignWithMargins = True
          Left = 512
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Light'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Light'
          TabOrder = 6
        end
        object btn_DisabledDark: TStyledButton
          AlignWithMargins = True
          Left = 577
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Dark'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Dark'
          TabOrder = 7
        end
      end
      object gbBootstrapModalResult: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 198
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Buttons with ModalResult'
        TabOrder = 4
        object btn_BootstrapOK: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Ok'
          ModalResult = 1
          StyleFamily = 'Bootstrap'
          TabOrder = 0
        end
        object btn_BootstrapCancel: TStyledButton
          AlignWithMargins = True
          Left = 85
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Cancel'
          ModalResult = 2
          StyleFamily = 'Bootstrap'
          TabOrder = 1
        end
        object btn_BootstrapAbort: TStyledButton
          AlignWithMargins = True
          Left = 165
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Abort'
          ModalResult = 3
          StyleFamily = 'Bootstrap'
          TabOrder = 2
        end
        object btn_BootstrapRetry: TStyledButton
          AlignWithMargins = True
          Left = 245
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Retry'
          ModalResult = 4
          StyleFamily = 'Bootstrap'
          TabOrder = 3
        end
        object btn_BootstrapIgnore: TStyledButton
          AlignWithMargins = True
          Left = 325
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Ignore'
          ModalResult = 5
          StyleFamily = 'Bootstrap'
          TabOrder = 4
        end
        object btn_BootstrapYes: TStyledButton
          AlignWithMargins = True
          Left = 405
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Yes'
          ModalResult = 6
          StyleFamily = 'Bootstrap'
          TabOrder = 5
        end
        object btn_BootstrapNo: TStyledButton
          AlignWithMargins = True
          Left = 485
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'No'
          ModalResult = 7
          StyleFamily = 'Bootstrap'
          TabOrder = 6
        end
        object btn_BootstrapClose: TStyledButton
          AlignWithMargins = True
          Left = 565
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Close'
          ModalResult = 8
          StyleFamily = 'Bootstrap'
          StyleAppearance = 'Outline'
          TabOrder = 7
        end
        object btn_BootstrapHelp: TStyledButton
          AlignWithMargins = True
          Left = 645
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Help'
          ModalResult = 9
          StyleFamily = 'Bootstrap'
          StyleClass = 'Warning'
          StyleAppearance = 'Outline'
          TabOrder = 8
        end
        object btn_BootstrapAll: TStyledButton
          AlignWithMargins = True
          Left = 725
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'All'
          ModalResult = 12
          StyleFamily = 'Bootstrap'
          TabOrder = 9
        end
      end
    end
    object tsAngular: TTabSheet
      Caption = 'Angular Buttons'
      ImageIndex = 19
      object AngularLinkLabel: TLinkLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 350
        Height = 17
        Caption = 
          'Similar to: <A HREF="https://material.angular.io/components/butt' +
          'on/overview">https://material.angular.io/components/button/overv' +
          'iew</A>'
        TabOrder = 0
        OnLinkClick = LinkLabelLinkClick
      end
      object gbAngularBasic: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 59
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Basic Buttons'
        TabOrder = 2
        object btn_BasicBasic: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          StyleAppearance = 'Basic'
          TabOrder = 0
        end
        object btn_BasicPrimary: TStyledButton
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
          TabOrder = 1
        end
        object btn_BasicAccent: TStyledButton
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Basic'
          TabOrder = 2
        end
        object btn_BasicWarn: TStyledButton
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Basic'
          TabOrder = 3
        end
        object btn_BasicDisabled: TStyledButton
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          StyleAppearance = 'Basic'
          TabOrder = 4
        end
      end
      object gbAngularRaised: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 124
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Raised Buttons'
        TabOrder = 3
        object btn_RaisedBasic: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          StyleAppearance = 'Raised'
          TabOrder = 0
        end
        object btn_RaisedPrimary: TStyledButton
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Raised'
          TabOrder = 1
        end
        object btn_RaisedAccent: TStyledButton
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Raised'
          TabOrder = 2
        end
        object btn_RaisedWarn: TStyledButton
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Raised'
          TabOrder = 3
        end
        object btn_RaisedDisabled: TStyledButton
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          StyleAppearance = 'Raised'
          TabOrder = 4
        end
      end
      object AngularThemesPanel: TPanel
        Left = 0
        Top = 0
        Width = 887
        Height = 56
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        OnResize = AngularThemesPanelResize
        object rgAngularLightThemes: TRadioGroup
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 407
          Height = 50
          Align = alLeft
          Caption = 'Angular Light Themes'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Deep Purple && Amber'
            'Indigo && Pink')
          TabOrder = 0
          OnClick = rgAngularLightThemesClick
        end
        object rgAngularDarkThemes: TRadioGroup
          AlignWithMargins = True
          Left = 416
          Top = 3
          Width = 468
          Height = 50
          Align = alClient
          Caption = 'Angular Dark Themes'
          Columns = 2
          Items.Strings = (
            'Pink && Blue-grey'
            'Purple && Green')
          TabOrder = 1
          OnClick = rgAngularDarkThemesClick
        end
      end
      object gpAngularStroked: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 189
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Stroked Buttons'
        TabOrder = 4
        object btn_StrokedBasic: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          StyleAppearance = 'Stroked'
          TabOrder = 0
        end
        object btn_StrokedPrimary: TStyledButton
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Stroked'
          TabOrder = 1
        end
        object btn_StrokedWarn: TStyledButton
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Stroked'
          TabOrder = 3
        end
        object btn_StrokedDisabled: TStyledButton
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          StyleAppearance = 'Stroked'
          TabOrder = 4
        end
        object btn_StrokedAccent: TStyledButton
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Stroked'
          TabOrder = 2
        end
      end
      object gbAngularFlat: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 254
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Flat Buttons'
        TabOrder = 5
        object btn_FlatBasic: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          TabOrder = 0
        end
        object btn_FlatPrimary: TStyledButton
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          TabOrder = 1
        end
        object btn_FlatWarn: TStyledButton
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          TabOrder = 3
        end
        object btn_FlatDisabled: TStyledButton
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          TabOrder = 4
        end
        object btn_FlatAccent: TStyledButton
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          TabOrder = 2
        end
      end
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 319
        Width = 881
        Height = 68
        Align = alTop
        Caption = 'Icon Buttons'
        TabOrder = 6
        object btn_IconHome: TStyledButton
          AlignWithMargins = True
          Left = 110
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 0
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
          TabOrder = 1
        end
        object btn_IconDots: TStyledButton
          AlignWithMargins = True
          Left = 22
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 10
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
          TabOrder = 0
        end
        object btn_IconMenu: TStyledButton
          AlignWithMargins = True
          Left = 198
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 4
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Basic'
          TabOrder = 2
        end
        object btn_IconHeart: TStyledButton
          AlignWithMargins = True
          Left = 286
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 8
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Basic'
          TabOrder = 3
        end
        object btn_IconLaunchDisabled: TStyledButton
          AlignWithMargins = True
          Left = 374
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Enabled = False
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 12
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleAppearance = 'Basic'
          TabOrder = 4
        end
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 393
        Width = 881
        Height = 87
        Align = alTop
        Caption = 'FAB'
        TabOrder = 7
        object btn_FABTrash: TStyledButton
          AlignWithMargins = True
          Left = 22
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 13
          StyleRadius = 8
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          TabOrder = 1
        end
        object btn_FABBookmark: TStyledButton
          AlignWithMargins = True
          Left = 126
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 16
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          TabOrder = 2
        end
        object btn_FABHome: TStyledButton
          AlignWithMargins = True
          Left = 230
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 17
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          TabOrder = 3
        end
        object btn_FABHeartDisabled: TStyledButton
          AlignWithMargins = True
          Left = 334
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Enabled = False
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 8
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          TabOrder = 0
        end
      end
      object gbAngularModalResult: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 486
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Buttons with ModalResult'
        TabOrder = 8
        object btn_AngularOK: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Ok'
          ModalResult = 1
          StyleFamily = 'Angular-Light'
          TabOrder = 0
        end
        object btn_AngularCancel: TStyledButton
          AlignWithMargins = True
          Left = 85
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Cancel'
          ModalResult = 2
          StyleFamily = 'Angular-Light'
          TabOrder = 1
        end
        object btn_AngularAbort: TStyledButton
          AlignWithMargins = True
          Left = 165
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Abort'
          ModalResult = 3
          StyleFamily = 'Angular-Light'
          TabOrder = 2
        end
        object btn_AngularRetry: TStyledButton
          AlignWithMargins = True
          Left = 245
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Retry'
          ModalResult = 4
          StyleFamily = 'Angular-Light'
          TabOrder = 3
        end
        object btn_AngularIgnore: TStyledButton
          AlignWithMargins = True
          Left = 325
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Ignore'
          ModalResult = 5
          StyleFamily = 'Angular-Light'
          TabOrder = 5
        end
        object btn_AngularYes: TStyledButton
          AlignWithMargins = True
          Left = 405
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Yes'
          ModalResult = 6
          StyleFamily = 'Angular-Light'
          TabOrder = 6
        end
        object btn_AngularNo: TStyledButton
          AlignWithMargins = True
          Left = 485
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'No'
          ModalResult = 7
          StyleFamily = 'Angular-Light'
          TabOrder = 7
        end
        object btn_AngularClose: TStyledButton
          AlignWithMargins = True
          Left = 565
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Close'
          ModalResult = 8
          StyleFamily = 'Angular-Light'
          TabOrder = 8
        end
        object btn_AngularHelp: TStyledButton
          AlignWithMargins = True
          Left = 645
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Help'
          ModalResult = 9
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          TabOrder = 9
        end
        object btn_AngularAll: TStyledButton
          AlignWithMargins = True
          Left = 725
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'All'
          ModalResult = 12
          StyleFamily = 'Angular-Light'
          TabOrder = 4
        end
      end
    end
    object tsClassic: TTabSheet
      Caption = 'Classic Buttons (VCL Styled)'
      ImageIndex = 21
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 887
        Height = 193
        Align = alTop
        Caption = 'Standard Delphi Button compared to StyleButton'
        TabOrder = 0
        object StyledButton: TStyledButton
          Left = 264
          Top = 22
          Width = 201
          Height = 37
          Hint = 
            'Classic TStyledButton (VCL Styled) with StyleElements: [seFont,s' +
            'eClient]'
          OnClick = ButtonClick
          PopUpMenu = PopupMenu
          Caption = 'TStyledButton (Application Styled)'
          TabOrder = 1
        end
        object StyledButtonDisable: TStyledButton
          Left = 264
          Top = 65
          Width = 201
          Height = 37
          Hint = 'Disabled Classic TStyledButton (Application Styled)'
          Enabled = False
          PopUpMenu = PopupMenu
          Caption = 'Disabled TStyledButton'
          TabOrder = 3
        end
        object StyledButtonStyled: TStyledButton
          Left = 264
          Top = 108
          Width = 201
          Height = 37
          Hint = 'Classic Button (Custom VCL Styled=Windows10 Blue)'
          OnClick = ButtonClick
          PopUpMenu = PopupMenu
          StyleElements = [seFont, seBorder]
          Caption = 'TStyledButton (Windows 10 Blue)'
          StyleClass = 'Windows10 Blue'
          TabOrder = 6
        end
        object StyledButtonSplit: TStyledButton
          Left = 264
          Top = 151
          Width = 201
          Height = 37
          Hint = 
            'Classic TStyledButton (VCL Styled) with StyleElements: [seFont,s' +
            'eClient]'
          OnClick = ButtonClick
          PopUpMenu = PopupMenu
          Caption = 'Styled "SplitButton"'
          DropDownMenu = PopupMenu
          Style = bsSplitButton
          TabOrder = 7
        end
        object VCLButton: TButton
          Left = 24
          Top = 22
          Width = 201
          Height = 37
          Hint = 'VCL TButton (use Global application VCL Style)'
          Caption = 'TButton (Application Styled)'
          Images = VirtualImageList32
          ModalResult = 1
          PopupMenu = PopupMenu
          TabOrder = 0
          OnClick = ButtonClick
        end
        object VCLButtonDisabled: TButton
          Left = 24
          Top = 65
          Width = 201
          Height = 37
          Caption = 'Disabled TButton'
          Enabled = False
          Images = VirtualImageList32
          ModalResult = 1
          PopupMenu = PopupMenu
          TabOrder = 2
        end
        object VCLButtonStyled: TButton
          Left = 24
          Top = 108
          Width = 201
          Height = 37
          Hint = 'VCL Button With StyleName=Windows10 Blue)'
          Caption = 'VCL Button (Windows10 Blue)'
          Images = VirtualImageList32
          ModalResult = 1
          PopupMenu = PopupMenu
          TabOrder = 4
          OnClick = ButtonClick
        end
        object ButtonSplit: TButton
          Left = 24
          Top = 151
          Width = 201
          Height = 37
          Hint = 'VCL TButton (use Global application VCL Style)'
          Caption = '"SplitButton"'
          DropDownMenu = PopupMenu
          Images = VirtualImageList32
          ModalResult = 1
          PopupMenu = PopupMenu
          Style = bsSplitButton
          TabOrder = 5
          OnClick = ButtonClick
        end
      end
      object gbClassicModalResult: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 196
        Width = 881
        Height = 59
        Align = alTop
        Caption = 'Buttons with ModalResult'
        TabOrder = 1
        object btn_ClassicOK: TStyledButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Ok'
          ModalResult = 1
          TabOrder = 0
        end
        object btn_ClassicCancel: TStyledButton
          AlignWithMargins = True
          Left = 85
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 1
        end
        object btn_ClassicAbort: TStyledButton
          AlignWithMargins = True
          Left = 165
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Abort'
          ModalResult = 3
          TabOrder = 2
        end
        object btn_ClassicRetry: TStyledButton
          AlignWithMargins = True
          Left = 245
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Retry'
          ModalResult = 4
          TabOrder = 3
        end
        object btn_ClassicIgnore: TStyledButton
          AlignWithMargins = True
          Left = 325
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Ignore'
          ModalResult = 5
          TabOrder = 4
        end
        object btn_ClassicYes: TStyledButton
          AlignWithMargins = True
          Left = 405
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Yes'
          ModalResult = 6
          TabOrder = 5
        end
        object btn_ClassicNo: TStyledButton
          AlignWithMargins = True
          Left = 485
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'No'
          ModalResult = 7
          TabOrder = 6
        end
        object btn_ClassicClose: TStyledButton
          AlignWithMargins = True
          Left = 565
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Close'
          ModalResult = 8
          TabOrder = 7
        end
        object btn_ClassicHelp: TStyledButton
          AlignWithMargins = True
          Left = 645
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'Help'
          ModalResult = 9
          TabOrder = 8
        end
        object btn_ClassicAll: TStyledButton
          AlignWithMargins = True
          Left = 725
          Top = 18
          Width = 74
          Height = 36
          Align = alLeft
          OnClick = ButtonClick
          Caption = 'All'
          ModalResult = 12
          TabOrder = 9
        end
      end
      object ClassicScrollBox: TScrollBox
        Left = 0
        Top = 258
        Width = 887
        Height = 318
        Align = alClient
        TabOrder = 2
        object GroupBox4: TGroupBox
          Left = 0
          Top = 0
          Width = 883
          Height = 47
          Align = alTop
          Caption = 'Normal Classic Buttons (similar to VCL Styled Button)'
          TabOrder = 0
          object ClassicNormalFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 143
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object GroupBox5: TGroupBox
          Left = 0
          Top = 47
          Width = 883
          Height = 56
          Align = alTop
          Caption = 'Outline Classic Buttons (inspired to VCL Styled Button)'
          TabOrder = 1
          object ClassicOutlineFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 143
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
    end
    object tsBasicColor: TTabSheet
      Caption = 'Basic-Color Buttons'
      ImageIndex = 22
      object BasicColorScrollBox: TScrollBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 881
        Height = 570
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 0
        OnMouseWheel = ScrollBoxMouseWheel
        object GroupBoxNormal: TGroupBox
          Left = 0
          Top = 0
          Width = 881
          Height = 185
          Align = alTop
          Caption = 'Normal Buttons'
          TabOrder = 0
          object FlowPanelNormal: TFlowPanel
            Left = 2
            Top = 15
            Width = 877
            Height = 143
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object GroupBoxOutline: TGroupBox
          Left = 0
          Top = 185
          Width = 881
          Height = 185
          Align = alTop
          Caption = 'Outline Buttons'
          TabOrder = 1
          object FlowPanelOutLine: TFlowPanel
            Left = 2
            Top = 15
            Width = 877
            Height = 143
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
    end
    object tsSVGColor: TTabSheet
      Caption = 'SVG-Color Buttons'
      ImageIndex = 23
      object SvgColorScrollBox: TScrollBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 881
        Height = 570
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 0
        object SvgColorNormalGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 881
          Height = 185
          Align = alTop
          Caption = 'Normal Buttons'
          TabOrder = 0
          object SvgColorNormalFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 877
            Height = 143
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object SvgColorOutlineGroupBox: TGroupBox
          Left = 0
          Top = 185
          Width = 881
          Height = 185
          Align = alTop
          Caption = 'Outline Buttons'
          TabOrder = 1
          object SvgColorOutlineFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 877
            Height = 143
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
    end
  end
  object ActionList: TActionList
    Images = VirtualImageList32
    Left = 550
    Top = 463
    object TestAction: TAction
      Caption = 'Show Editor'
      Hint = 'Hint of Action'
      ImageIndex = 22
      OnExecute = TestActionExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 566
    Top = 544
    object New1: TMenuItem
      Caption = '&New'
      OnClick = PopupMenuClick
    end
    object Open1: TMenuItem
      Caption = '&Open...'
      OnClick = PopupMenuClick
    end
    object Save1: TMenuItem
      Caption = '&Save'
      OnClick = PopupMenuClick
    end
    object SaveAs1: TMenuItem
      Caption = 'Save &As...'
      OnClick = PopupMenuClick
    end
    object Exit1: TMenuItem
      Caption = 'E&xit'
      OnClick = PopupMenuClick
    end
  end
  object VirtualImageList32: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'home-deeppurple'
        Disabled = False
        Name = 'home-deeppurple'
      end
      item
        CollectionIndex = 1
        CollectionName = 'home-indigo'
        Disabled = False
        Name = 'home-indigo'
      end
      item
        CollectionIndex = 2
        CollectionName = 'home-pink'
        Name = 'home-pink'
      end
      item
        CollectionIndex = 3
        CollectionName = 'home-purple'
        Name = 'home-purple'
      end
      item
        CollectionIndex = 4
        CollectionName = 'menu-amber'
        Name = 'menu-amber'
      end
      item
        CollectionIndex = 5
        CollectionName = 'menu-pink'
        Name = 'menu-pink'
      end
      item
        CollectionIndex = 6
        CollectionName = 'menu-Blue-grey'
        Name = 'menu-Blue-grey'
      end
      item
        CollectionIndex = 7
        CollectionName = 'menu-green'
        Name = 'menu-green'
      end
      item
        CollectionIndex = 8
        CollectionName = 'heart'
        Name = 'heart'
      end
      item
        CollectionIndex = 9
        CollectionName = 'dots-vertical-white'
        Name = 'dots-vertical-white'
      end
      item
        CollectionIndex = 10
        CollectionName = 'dots-vertical-black'
        Name = 'dots-vertical-black'
      end
      item
        CollectionIndex = 11
        CollectionName = 'launch-white'
        Name = 'launch-white'
      end
      item
        CollectionIndex = 12
        CollectionName = 'launch-black'
        Name = 'launch-black'
      end
      item
        CollectionIndex = 13
        CollectionName = 'trash-white'
        Name = 'trash-white'
      end
      item
        CollectionIndex = 14
        CollectionName = 'trash-black'
        Name = 'trash-black'
      end
      item
        CollectionIndex = 15
        CollectionName = 'bookmark-white'
        Name = 'bookmark-white'
      end
      item
        CollectionIndex = 16
        CollectionName = 'bookmark-black'
        Name = 'bookmark-black'
      end
      item
        CollectionIndex = 17
        CollectionName = 'home-white'
        Name = 'home-white'
      end
      item
        CollectionIndex = 18
        CollectionName = 'home-black'
        Name = 'home-black'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Angular-logo'
        Name = 'Angular-logo'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Bootstrap-logo'
        Name = 'Bootstrap-logo'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Delphi-Logo'
        Name = 'Delphi-Logo'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Basic-Color'
        Name = 'Basic-Color'
      end
      item
        CollectionIndex = 23
        CollectionName = 'SVG-Color'
        Name = 'SVG-Color'
      end>
    ImageCollection = dmResources.ImageCollection
    PreserveItems = True
    Width = 32
    Height = 32
    Left = 460
    Top = 542
  end
end
