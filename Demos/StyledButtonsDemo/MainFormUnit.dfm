object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Styled Buttons (c) Copyright Ethea S.r.l.'
  ClientHeight = 691
  ClientWidth = 874
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 874
    Height = 70
    Align = alTop
    TabOrder = 0
    object StyledButton: TStyledButton
      Tag = 0
      Left = 10
      Top = 14
      Width = 172
      Height = 40
      Action = TestAction
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object StyledButton2: TStyledButton
      Tag = 0
      Left = 279
      Top = 9
      Width = 50
      Height = 50
      Hint = 'Hint of Action'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Caption = ''
      ImageAlignment = iaCenter
      Images = VirtualImageList32
      ImageIndex = 18
      ImageName = 'home-black'
      StyleDrawType = btEllipse
      StyleFamily = 'Bootstrap'
      StyleClass = 'Warning'
      StyleAppearance = 'Outline'
      TabOrder = 2
    end
    object StyleLabel: TLabel
      Left = 564
      Top = 14
      Width = 130
      Height = 13
      Caption = 'Change application style:'
    end
    object StyledButton1: TStyledButton
      Tag = 0
      Left = 203
      Top = 9
      Width = 54
      Height = 50
      Hint = 'Hint of Action'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Caption = ''
      ImageAlignment = iaCenter
      Images = VirtualImageList32
      ImageIndex = 11
      ImageName = 'launch-white'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Danger'
      TabOrder = 1
    end
    object Button1: TButton
      Left = 379
      Top = 17
      Width = 134
      Height = 37
      Caption = 'Vcl Button'
      ImageName = 'approval'
      PopupMenu = PopupMenu
      TabOrder = 3
    end
    object cbChangeStyle: TComboBox
      Left = 564
      Top = 33
      Width = 190
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnSelect = cbChangeStyleSelect
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 70
    Width = 874
    Height = 621
    ActivePage = tsBootstrap
    Align = alClient
    TabOrder = 1
    object tsBootstrap: TTabSheet
      Caption = 'Bootstrap Buttons'
      object BootStrapLinkLabel: TLinkLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 860
        Height = 17
        Align = alTop
        Caption = 
          'Similar to: <A HREF="https://getbootstrap.com/docs/4.0/component' +
          's/buttons/">https://getbootstrap.com/docs/4.0/components/buttons' +
          '/</A>'
        TabOrder = 0
        OnLinkClick = LinkLabelLinkClick
        ExplicitWidth = 352
      end
      object gbBootstrapNormal: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 26
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Normal Buttons'
        TabOrder = 1
        object btn_Primary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Primary'
          TabOrder = 1
          ExplicitLeft = 13
          ExplicitTop = 27
          ExplicitHeight = 34
        end
        object btn_Secondary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Secondary'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Secondary'
          TabOrder = 2
        end
        object btn_Success: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 181
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Caption = 'Success'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Success'
          TabOrder = 3
          ExplicitLeft = 239
          ExplicitTop = 20
        end
        object btn_Danger: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 279
          Top = 18
          Height = 36
          Align = alLeft
          Caption = 'Danger'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Danger'
          TabOrder = 4
        end
        object btn_Warning: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 360
          Top = 18
          Width = 81
          Height = 36
          Align = alLeft
          Caption = 'Warning'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Warning'
          TabOrder = 5
        end
        object btn_Info: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 447
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Info'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Info'
          TabOrder = 6
        end
        object btn_Light: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 512
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Light'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Light'
          TabOrder = 7
          ExplicitLeft = 557
          ExplicitTop = 20
        end
        object btn_Dark: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 577
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Caption = 'Dark'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Dark'
          TabOrder = 0
          ExplicitLeft = 628
          ExplicitTop = 20
        end
      end
      object gbBootstrapOutlined: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 91
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Outlined Buttons'
        TabOrder = 2
        object btn_OutlinePrimary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Caption = 'Primary'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Primary'
          StyleAppearance = 'Outline'
          TabOrder = 1
          ExplicitLeft = 13
          ExplicitTop = 27
          ExplicitHeight = 34
        end
        object btn_OutlineSecondary: TStyledButton
          Tag = 0
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
          TabOrder = 2
        end
        object btn_OutlineSuccess: TStyledButton
          Tag = 0
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
          TabOrder = 3
          ExplicitLeft = 239
          ExplicitTop = 20
        end
        object btn_OutlineDanger: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 279
          Top = 18
          Height = 36
          Align = alLeft
          Caption = 'Danger'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Danger'
          StyleAppearance = 'Outline'
          TabOrder = 4
        end
        object btn_OutlineWarning: TStyledButton
          Tag = 0
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
          TabOrder = 5
        end
        object btn_OutlineInfo: TStyledButton
          Tag = 0
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
          TabOrder = 6
        end
        object btn_OutlineLight: TStyledButton
          Tag = 0
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
          TabOrder = 7
          ExplicitLeft = 557
          ExplicitTop = 20
        end
        object btn_OutlineDark: TStyledButton
          Tag = 0
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
          TabOrder = 0
          ExplicitLeft = 628
          ExplicitTop = 20
        end
      end
      object gbBuutstrapDisabled: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 156
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Disabled Buttons'
        TabOrder = 3
        object btn_DisabledPrimary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Primary'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Primary'
          TabOrder = 1
          ExplicitLeft = 13
          ExplicitTop = 27
          ExplicitHeight = 34
        end
        object btn_DisabledSecondary: TStyledButton
          Tag = 0
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
          TabOrder = 2
        end
        object btn_DisabledSuccess: TStyledButton
          Tag = 0
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
          TabOrder = 3
          ExplicitLeft = 239
          ExplicitTop = 20
        end
        object btn_DisabledDanger: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 279
          Top = 18
          Height = 36
          Align = alLeft
          Enabled = False
          Caption = 'Danger'
          StyleFamily = 'Bootstrap'
          StyleClass = 'Danger'
          TabOrder = 4
        end
        object btn_DisabledWarning: TStyledButton
          Tag = 0
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
          TabOrder = 5
        end
        object btn_DisabledInfo: TStyledButton
          Tag = 0
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
          TabOrder = 6
        end
        object btn_DisabledLight: TStyledButton
          Tag = 0
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
          TabOrder = 7
          ExplicitLeft = 557
          ExplicitTop = 20
        end
        object btn_DisabledDark: TStyledButton
          Tag = 0
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
          TabOrder = 0
          ExplicitLeft = 628
          ExplicitTop = 20
        end
      end
    end
    object tsAngular: TTabSheet
      Caption = 'Angular Buttons'
      object AngularLinkLabel: TLinkLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 860
        Height = 17
        Align = alTop
        Caption = 
          'Similar to: <A HREF="https://material.angular.io/components/butt' +
          'on/overview">https://material.angular.io/components/button/overv' +
          'iew</A>'
        TabOrder = 0
        OnLinkClick = LinkLabelLinkClick
        ExplicitWidth = 350
      end
      object gbAngularBasic: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 82
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Basic Buttons'
        TabOrder = 2
        ExplicitWidth = 856
        object btn_BasicBasic: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Basic'
          TabOrder = 0
          ExplicitLeft = 13
          ExplicitTop = 27
          ExplicitHeight = 34
        end
        object btn_BasicPrimary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
          TabOrder = 1
        end
        object btn_BasicAccent: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Basic'
          TabOrder = 2
          ExplicitLeft = 239
          ExplicitTop = 20
        end
        object btn_BasicWarn: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Basic'
          TabOrder = 3
          ExplicitLeft = 447
        end
        object btn_BasicDisabled: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Basic'
          TabOrder = 4
          ExplicitLeft = 512
        end
      end
      object gbAngularRaised: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 147
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Raised Buttons'
        TabOrder = 3
        ExplicitWidth = 856
        object btn_RaisedBasic: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Raised'
          TabOrder = 0
          ExplicitLeft = 13
          ExplicitTop = 27
          ExplicitHeight = 34
        end
        object btn_RaisedPrimary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Raised'
          TabOrder = 1
        end
        object btn_RaisedAccent: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Raised'
          TabOrder = 2
          ExplicitLeft = 239
          ExplicitTop = 20
        end
        object btn_RaisedWarn: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Raised'
          TabOrder = 3
          ExplicitLeft = 447
        end
        object btn_RaisedDisabled: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Raised'
          TabOrder = 4
          ExplicitLeft = 512
        end
      end
      object AngularThemesPanel: TPanel
        Left = 0
        Top = 23
        Width = 866
        Height = 56
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        OnResize = AngularThemesPanelResize
        ExplicitWidth = 862
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
          Width = 447
          Height = 50
          Align = alClient
          Caption = 'Angular Dark Themes'
          Columns = 2
          Items.Strings = (
            'Pink && Blue-grey'
            'Purple && Green')
          TabOrder = 1
          OnClick = rgAngularDarkThemesClick
          ExplicitWidth = 443
        end
      end
      object gpAngularStroked: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 212
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Stroked Buttons'
        TabOrder = 4
        ExplicitWidth = 856
        object btn_StrokedBasic: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Stroked'
          TabOrder = 0
          ExplicitLeft = 13
          ExplicitTop = 27
          ExplicitHeight = 34
        end
        object btn_StrokedPrimary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Stroked'
          TabOrder = 1
        end
        object btn_StrokedWarn: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Stroked'
          TabOrder = 3
          ExplicitLeft = 447
        end
        object btn_StrokedDisabled: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Stroked'
          TabOrder = 4
          ExplicitLeft = 512
        end
        object btn_StrokedAccent: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Stroked'
          TabOrder = 2
          ExplicitLeft = 239
          ExplicitTop = 20
        end
      end
      object gbAngularFlat: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 277
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Flat Buttons'
        TabOrder = 5
        ExplicitWidth = 856
        object btn_FlatBasic: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 72
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Basic'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Flat'
          TabOrder = 0
          ExplicitLeft = 13
          ExplicitTop = 27
          ExplicitHeight = 34
        end
        object btn_FlatPrimary: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 83
          Top = 18
          Width = 102
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Primary'
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Flat'
          TabOrder = 1
        end
        object btn_FlatWarn: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 289
          Top = 18
          Width = 59
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Warn'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Flat'
          TabOrder = 3
          ExplicitLeft = 447
        end
        object btn_FlatDisabled: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 354
          Top = 18
          Width = 80
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Disabled'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Flat'
          TabOrder = 4
          ExplicitLeft = 512
        end
        object btn_FlatAccent: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 191
          Top = 18
          Width = 92
          Height = 36
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = 'Accent'
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Flat'
          TabOrder = 2
          ExplicitLeft = 239
          ExplicitTop = 20
        end
      end
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 342
        Width = 860
        Height = 68
        Align = alTop
        Caption = 'Icon Buttons'
        TabOrder = 6
        object btn_IconHome: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 110
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 0
          ImageName = 'home-deeppurple'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
          TabOrder = 0
          ExplicitLeft = 75
          ExplicitHeight = 36
        end
        object btn_IconDots: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 22
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 10
          ImageName = 'dots-vertical-black'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
          TabOrder = 1
          ExplicitLeft = 5
          ExplicitHeight = 36
        end
        object btn_IconMenu: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 198
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 4
          ImageName = 'menu-amber'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Basic'
          TabOrder = 2
          ExplicitLeft = 145
          ExplicitHeight = 36
        end
        object btn_IconHeart: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 286
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 8
          ImageName = 'heart'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Basic'
          TabOrder = 3
          ExplicitLeft = 215
          ExplicitHeight = 36
        end
        object btn_IconLaunchDisabled: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 374
          Top = 18
          Width = 48
          Height = 45
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 12
          ImageName = 'launch-black'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Basic'
          TabOrder = 4
          ExplicitLeft = 285
          ExplicitHeight = 36
        end
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 416
        Width = 843
        Height = 87
        Margins.Right = 20
        Align = alTop
        Caption = 'FAB'
        TabOrder = 7
        ExplicitLeft = -14
        ExplicitTop = 453
        ExplicitWidth = 860
        object btn_FABTrash: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 22
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 13
          ImageName = 'trash-white'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Flat'
          TabOrder = 3
          ExplicitLeft = 13
          ExplicitTop = 23
        end
        object btn_FABBookmark: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 126
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 16
          ImageName = 'bookmark-black'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
          StyleAppearance = 'Flat'
          TabOrder = 2
          ExplicitLeft = 162
          ExplicitTop = 20
        end
        object btn_FABHome: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 230
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 17
          ImageName = 'home-white'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Flat'
          TabOrder = 1
          ExplicitLeft = 313
          ExplicitTop = 3
        end
        object btn_FABHeartDisabled: TStyledButton
          Tag = 0
          AlignWithMargins = True
          Left = 334
          Top = 18
          Width = 64
          Height = 64
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Caption = ''
          ImageAlignment = iaCenter
          Images = VirtualImageList32
          ImageIndex = 8
          ImageName = 'heart'
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Basic'
          StyleAppearance = 'Flat'
          TabOrder = 0
          ExplicitLeft = 413
          ExplicitTop = 20
        end
      end
    end
    object tsClassic: TTabSheet
      Caption = 'Classic'
      ImageIndex = 2
    end
  end
  object ActionList: TActionList
    Left = 550
    Top = 463
    object TestAction: TAction
      Caption = 'Show Editor'
      Hint = 'Hint of Action'
      ImageName = 'approval'
      OnExecute = TestActionExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 566
    Top = 544
    object New1: TMenuItem
      Caption = '&New'
    end
    object Open1: TMenuItem
      Caption = '&Open...'
    end
    object Save1: TMenuItem
      Caption = '&Save'
    end
    object SaveAs1: TMenuItem
      Caption = 'Save &As...'
    end
    object Exit1: TMenuItem
      Caption = 'E&xit'
    end
  end
  object VirtualImageList32: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'home-deeppurple'
        Name = 'home-deeppurple'
      end
      item
        CollectionIndex = 1
        CollectionName = 'home-indigo'
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
      end>
    ImageCollection = dmResources.ImageCollection
    PreserveItems = True
    Width = 32
    Height = 32
    Left = 460
    Top = 542
  end
end
