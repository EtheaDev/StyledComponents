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
      Images = ImageList32
      ImageIndex = 18
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
      Images = ImageList32
      ImageIndex = 11
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
        Top = 68
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
        Top = 133
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
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Basic Buttons'
        TabOrder = 2
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
        Top = 124
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Raised Buttons'
        TabOrder = 3
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
        Top = 0
        Width = 866
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
        end
      end
      object gpAngularStroked: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 189
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Stroked Buttons'
        TabOrder = 4
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
        Top = 254
        Width = 860
        Height = 59
        Align = alTop
        Caption = 'Flat Buttons'
        TabOrder = 5
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
        Top = 319
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
          Images = ImageList32
          ImageIndex = 0
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
          Images = ImageList32
          ImageIndex = 10
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
          Images = ImageList32
          ImageIndex = 4
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
          Images = ImageList32
          ImageIndex = 8
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
          Images = ImageList32
          ImageIndex = 12
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
        Top = 393
        Width = 843
        Height = 87
        Margins.Right = 20
        Align = alTop
        Caption = 'FAB'
        TabOrder = 7
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
          Images = ImageList32
          ImageIndex = 13
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Flat'
          TabOrder = 1
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
          Images = ImageList32
          ImageIndex = 16
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
          Images = ImageList32
          ImageIndex = 17
          StyleDrawType = btEllipse
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Flat'
          TabOrder = 3
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
          Images = ImageList32
          ImageIndex = 8
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
  object ImageList32: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Height = 32
    Width = 32
    Left = 460
    Top = 542
    Bitmap = {
      494C010113001800340020002000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000080000000A000000001002000000000000040
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000061000000CB0000004300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000043000000CB00000061000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000F8000000700000
      0006000000000000000000000000000000000000000000000000000000000000
      00000000000600000070000000F8000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000012121245ADADADD2ADADADD2ADADADD2ADADADD2ADAD
      ADD2ADADADD2121212450000000000000000000000000000000012121245ADAD
      ADD2ADADADD2ADADADD2ADADADD2ADADADD2ADADADD212121245000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000045000000D2000000D2000000D2000000D20000
      00D2000000D20000004500000000000000000000000000000000000000450000
      00D2000000D2000000D2000000D2000000D2000000D200000045000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE0000009E0000001D000000000000000000000000000000000000001D0000
      009E000000FE000000FE000000FE000000FE000000FE00000069000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE2A2A2A68000000000000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE0000006800000000000000000000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000CB0000004300000043000000CB000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE2A2A2A68000000000000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE0000006800000000000000000000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE2A2A2A68000000000000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE0000006800000000000000000000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE2A2A2A68000000000000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE0000006800000000000000000000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE2A2A2A68000000000000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE0000006800000000000000000000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE2A2A2A68000000000000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE0000006800000000000000000000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE2A2A2A68000000000000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE0000006800000000000000000000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEEDEDEDF6ADADADD2ADADADD2ADADADD2ADADADD2EDEDEDF6FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000F6000000D2000000D2000000D2000000D2000000F60000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      00000000000000000000000000000000000000000000000000000000000ECFCF
      CFE6FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFECFCFCFE60000000E000000000000000000000000000000000000000E0000
      00E6000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000E60000000E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0006A1A1A1CBFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEA1A1
      A1CB000000060000000000000000000000000000000000000000000000000000
      0006000000CB000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00CB000000060000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000757575ADFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE757575AD0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000AD000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000AD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004F4F4F8EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE4F4F4F8E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000008E000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE0000008E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000031313170FDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE3131317000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000070000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE0000007000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001C1C1C55FDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFE1C1C1C550000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000055000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000550000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000F0F0F3EFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFE0F0F0F3E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000003E000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE0000003E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000707072BFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFE0707072B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002B0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE0000002B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000202
      021BFBFBFBFDFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFBFB
      FBFD0202021B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      001B000000FD000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FD0000001B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000062000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000061000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000ECFCFCFE6FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFECFCFCFE60000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000E000000E6000000FE000000FE000000FE000000FE000000E60000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000001A000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000018000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000006A1A1A1CBFDFDFDFEFDFDFDFEA1A1A1CB000000060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000006000000CB000000FE000000FE000000CB000000060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000054000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE0000005200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000757575AD757575AD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000AD000000AD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000001B000000DF000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000DF0000
      001B000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001B1B1B54FDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE1B1B1B5400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000054000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE0000005400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000024242461A1A1A1CB1111114300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000011111143A1A1A1CB24242461000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000DF000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00DF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000202021AFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE0202021A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000001A000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE0000001A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEF1F1F1F8313131700000
      0006000000000000000000000000000000000000000000000000000000000000
      00000000000631313170F1F1F1F8FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000FE000000D3000000D3000000D3000000D30000
      00D3000000D3000000D3000000D3000000D3000000D3000000D3000000D30000
      00D3000000D3000000D3000000D3000000D3000000D3000000FE000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000025252562FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE25252562000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000062000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000062000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE6262629E0303031D000000000000000000000000000000000303031D6262
      629EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2B2B2B69000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEA1A1A1CB1111114311111143A1A1A1CBFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE2B2B2B692B2B2B69787878AFFDFDFDFEFDFDFDFE787878AF2B2B2B692B2B
      2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE0000006900000069000000AF000000FE000000FE000000AF000000690000
      0069000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      001B000000690000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D20000000000000000000000000000001B0000
      00FE000000FE0000007A00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000690000
      00FE000000FE000000FE0000007A000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      007A000000FE000000FE000000FE0000007A0000000000000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      00000000007A000000FE000000FE000000FE0000007A00000000000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000007A000000FE000000FE000000FE0000007A000000000000
      00000000000000000000000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      000000000000000000000000007A000000FE000000FE000000FE0000007A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      00000000000000000000000000000000007A000000FE000000FE000000FE0000
      007A000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000007A000000FE000000FE0000
      00FE0000007A0000000000000000000000000000000000000045000000680000
      0068000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007A000000FE0000
      00FE000000FE0000007A000000000000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000007A0000
      00FE000000FE000000FE0000007A0000000000000000000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE00000000000000002B2B2B69FDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000069000000FE000000FE00000068000000000000
      0000000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      007A000000FE000000FE000000FE0000007A00000000000000D2000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEADADADD2ADADADD2EDEDEDF6FDFDFDFEFDFDFDFEEDEDEDF6ADADADD2ADAD
      ADD2FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000D2000000D2000000F6000000FE000000FE000000F6000000D20000
      00D2000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000007A000000FE000000FE000000FE0000007A000000D3000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000007A000000FE000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE00000068000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000D2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000007A000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000AEAEAED3FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEADADADD20000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000D3000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000D20000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B2B2B69FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE2A2A2A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000FE000000D2000000D2000000D2000000D20000
      00D2000000D2000000D2000000D2000000D20000000000000000000000450000
      00D2000000D2000000D2000000D2000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      000000000000AEAEAED3FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEADADADD20000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000D3000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000D20000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000025252562FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE24242461000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000DE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE0000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      0000000000004D4D4D8CAEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAE
      AED3FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEAEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAEAED34D4D4D8C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000008C000000D3000000D3000000D3000000D3000000D30000
      00D3000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000D3000000D3000000D3000000D3000000D3000000D30000008C0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000202021AFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE02020218000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000001B000000DE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000FE000000FE000000FE000000FE0000000000000000000000690000
      00FE000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FE000000FE000000FE000000FE000000FE000000FE000000FE0000
      00FE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001B1B1B54FDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE1A1A1A5200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000010105250101052500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000002020A353542F2FE3542F2FE02020A35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000202021BC3C3C3DFFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEC3C3C3DF0202
      021B000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000040513483542F2FE3542F2FE3542F2FE3542F2FE040513480000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F3E8C8C8CBD8C8C8CBD0F0F0F3E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003E000000BD000000BD0000003E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C3C3C3DFFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEC3C3
      C3DF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000709215E3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE0709
      215E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F3EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE0F0F0F3E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003E000000FE000000FE000000FE000000FE0000003E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEAEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAE
      AED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAE
      AED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3AEAEAED3FDFDFDFEFDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000B0D
      32743542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE0B0D32740000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C8C8CBDFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE8C8C8CBD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000BD000000FE000000FE000000FE000000FE000000BD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F13478A3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE0F13478A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C8C8CBDFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE8C8C8CBD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000BD000000FE000000FE000000FE000000FE000000BD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001318599A3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE1318599A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F3EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE0F0F0F3E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003E000000FE000000FE000000FE000000FE0000003E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000151A60A03542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE151A60A00000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F3E8C8C8CBD8C8C8CBD0F0F0F3E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003E000000BD000000BD0000003E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000202
      021B2B2B2B690000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000131757993542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE1317579900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD20000000000000000000000000202021BFDFD
      FDFEFDFDFDFE3A3A3A7A00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      000000000000000000000D113D803542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE0D113D80000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD20000000000000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFE3A3A3A7A000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      00000000000005071A553542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE05071A550000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F3E8C8C8CBD8C8C8CBD0F0F0F3E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003E000000BD000000BD0000003E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000003A3A
      3A7AFDFDFDFEFDFDFDFEFDFDFDFE3A3A3A7A0000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      00000000031F3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE0000
      031F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F3EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE0F0F0F3E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003E000000FE000000FE000000FE000000FE0000003E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      00003A3A3A7AFDFDFDFEFDFDFDFEFDFDFDFE3A3A3A7A00000000000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      00002731B4DB3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE2731
      B4DB000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C8C8CBDFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE8C8C8CBD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000BD000000FE000000FE000000FE000000FE000000BD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000003A3A3A7AFDFDFDFEFDFDFDFEFDFDFDFE3A3A3A7A000000000000
      00000000000000000000000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000506
      18503542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE050618500000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C8C8CBDFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE8C8C8CBD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000BD000000FE000000FE000000FE000000FE000000BD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      000000000000000000003A3A3A7AFDFDFDFEFDFDFDFEFDFDFDFE3A3A3A7A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002833
      BADF3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE2833BADF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F3EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE0F0F0F3E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003E000000FE000000FE000000FE000000FE0000003E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      00000000000000000000000000003A3A3A7AFDFDFDFEFDFDFDFEFDFDFDFE3A3A
      3A7A000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000F3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE0000000F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F3E8C8C8CBD8C8C8CBD0F0F0F3E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003E000000BD000000BD0000003E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000003A3A3A7AFDFDFDFEFDFDFDFEFDFD
      FDFE3A3A3A7A00000000000000000000000000000000121212452A2A2A682A2A
      2A680000000000000000000000000000000000000000000000000405144A3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE0405144A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      000000000000000000000000000000000000000000003A3A3A7AFDFDFDFEFDFD
      FDFEFDFDFDFE3A3A3A7A000000000000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000080A25643542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE080A256400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003A3A3A7AFDFD
      FDFEFDFDFDFEFDFDFDFE3A3A3A7A0000000000000000AEAEAED3FDFDFDFEFDFD
      FDFE00000000000000000000000000000000000000000000000006071B563542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE06071B5600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F3E8C8C8CBD8C8C8CBD0F0F0F3E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003E000000BD000000BD0000003E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003A3A
      3A7AFDFDFDFEFDFDFDFEFDFDFDFE3A3A3A7A00000000ADADADD2FDFDFDFEFDFD
      FDFE0000000000000000000000000000000000000000000000000000031F3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE0000031F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F3EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE0F0F0F3E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003E000000FE000000FE000000FE000000FE0000003E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003A3A3A7AFDFDFDFEFDFDFDFEFDFDFDFE3A3A3A7AAEAEAED3FDFDFDFEFDFD
      FDFE00000000000000000000000000000000000000000000000000000000313D
      DFF43542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE313DDFF40000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C8C8CBDFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE8C8C8CBD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000BD000000FE000000FE000000FE000000FE000000BD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003A3A3A7AFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000507
      1A553542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE05071A550000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C8C8CBDFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE8C8C8CBD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000BD000000FE000000FE000000FE000000FE000000BD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEADADADD2000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003A3A3A7AFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000151B62A23542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE2933BCE02933BCE03542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE151B
      62A2000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F3EFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE0F0F0F3E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003E000000FE000000FE000000FE000000FE0000003E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFDFEFDFDFDFEFDFDFDFEADADADD2ADADADD2ADADADD2ADADADD2ADAD
      ADD2ADADADD2ADADADD2ADADADD2ADADADD2000000000000000012121245ADAD
      ADD2ADADADD2ADADADD2ADADADD2FDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      00000000000010154E903542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542
      F2FE3542F2FE3542F2FE191F72AF0000000500000005191F72AF3542F2FE3542
      F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE3542F2FE10154E900000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F3E8C8C8CBD8C8C8CBD0F0F0F3E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003E000000BD000000BD0000003E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C1C1C1DEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE00000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      000000000000000000000102072D1A2078B33542F2FE3542F2FE3542F2FE3542
      F2FE1A2078B30102083100000000000000000000000000000000010208311A20
      78B33542F2FE3542F2FE3542F2FE3542F2FE1A2078B30102072D000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000202021BC1C1C1DEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFE00000000000000002B2B2B69FDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE000000000000000000000000000000000000000000000000000000000000
      00003FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000ACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FE000000000000000000000000000000000000000000000000000000000000
      0000621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE000000000000000000000000000000000000000000000000000000000000
      0000897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE000000000000000000000000000000000000000000000000000000000000
      00003FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000ACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FE000000000000000000000000000000000000000000000000000000000000
      0000621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE000000000000000000000000000000000000000000000000000000000000
      00005E5441D35E5441D35E5441D35E5441D35E5441D35E5441D35E5441D35E54
      41D35E5441D35E5441D35E5441D35E5441D35E5441D35E5441D35E5441D35E54
      41D35E5441D35E5441D35E5441D35E5441D35E5441D35E5441D35E5441D35E54
      41D3000000000000000000000000000000000000000000000000000000000000
      00002B93AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93
      AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93
      AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93AED32B93
      AED3000000000000000000000000000000000000000000000000000000000000
      000077A447D377A447D377A447D377A447D377A447D377A447D377A447D377A4
      47D377A447D377A447D377A447D377A447D377A447D377A447D377A447D377A4
      47D377A447D377A447D377A447D377A447D377A447D377A447D377A447D377A4
      47D3000000000000000000000000000000000000000000000000000000000000
      000043149FD343149FD343149FD343149FD343149FD343149FD343149FD34314
      9FD343149FD343149FD343149FD343149FD343149FD343149FD343149FD34314
      9FD343149FD343149FD343149FD343149FD343149FD343149FD343149FD34314
      9FD3000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000017140F6817140F6817140F6817140F6817140F6817140F6817140F681714
      0F6817140F6817140F6817140F6817140F6817140F6817140F6817140F681714
      0F6817140F6817140F6817140F6817140F6817140F6817140F6817140F681714
      0F68000000000000000000000000000000000000000000000000000000000000
      00000A232A680A232A680A232A680A232A680A232A680A232A680A232A680A23
      2A680A232A680A232A680A232A680A232A680A232A680A232A680A232A680A23
      2A680A232A680A232A680A232A680A232A680A232A680A232A680A232A680A23
      2A68000000000000000000000000000000000000000000000000000000000000
      00001C2711681C2711681C2711681C2711681C2711681C2711681C2711681C27
      11681C2711681C2711681C2711681C2711681C2711681C2711681C2711681C27
      11681C2711681C2711681C2711681C2711681C2711681C2711681C2711681C27
      1168000000000000000000000000000000000000000000000000000000000000
      0000100426681004266810042668100426681004266810042668100426681004
      2668100426681004266810042668100426681004266810042668100426681004
      2668100426681004266810042668100426681004266810042668100426681004
      2668000000000000000000000000000000000000000000000000000000000000
      0000897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE000000000000000000000000000000000000000000000000000000000000
      00003FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000ACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FE000000000000000000000000000000000000000000000000000000000000
      0000621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE000000000000000000000000000000000000000000000000000000000000
      0000897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE000000000000000000000000000000000000000000000000000000000000
      00003FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000ACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FE000000000000000000000000000000000000000000000000000000000000
      0000621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE000000000000000000000000000000000000000000000000000000000000
      0000171410691714106917141069171410691714106917141069171410691714
      1069171410691714106917141069171410691714106917141069171410691714
      1069171410691714106917141069171410691714106917141069171410691714
      1069000000000000000000000000000000000000000000000000000000000000
      00000A242B690A242B690A242B690A242B690A242B690A242B690A242B690A24
      2B690A242B690A242B690A242B690A242B690A242B690A242B690A242B690A24
      2B690A242B690A242B690A242B690A242B690A242B690A242B690A242B690A24
      2B69000000000000000000000000000000000000000000000000000000000000
      00001D2811691D2811691D2811691D2811691D2811691D2811691D2811691D28
      11691D2811691D2811691D2811691D2811691D2811691D2811691D2811691D28
      11691D2811691D2811691D2811691D2811691D2811691D2811691D2811691D28
      1169000000000000000000000000000000000000000000000000000000000000
      0000100427691004276910042769100427691004276910042769100427691004
      2769100427691004276910042769100427691004276910042769100427691004
      2769100427691004276910042769100427691004276910042769100427691004
      2769000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005D5440D25D5440D25D5440D25D5440D25D5440D25D5440D25D5440D25D54
      40D25D5440D25D5440D25D5440D25D5440D25D5440D25D5440D25D5440D25D54
      40D25D5440D25D5440D25D5440D25D5440D25D5440D25D5440D25D5440D25D54
      40D2000000000000000000000000000000000000000000000000000000000000
      00002B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92
      ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92
      ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92ADD22B92
      ADD2000000000000000000000000000000000000000000000000000000000000
      000076A346D276A346D276A346D276A346D276A346D276A346D276A346D276A3
      46D276A346D276A346D276A346D276A346D276A346D276A346D276A346D276A3
      46D276A346D276A346D276A346D276A346D276A346D276A346D276A346D276A3
      46D2000000000000000000000000000000000000000000000000000000000000
      000043149ED243149ED243149ED243149ED243149ED243149ED243149ED24314
      9ED243149ED243149ED243149ED243149ED243149ED243149ED243149ED24314
      9ED243149ED243149ED243149ED243149ED243149ED243149ED243149ED24314
      9ED2000000000000000000000000000000000000000000000000000000000000
      0000897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE000000000000000000000000000000000000000000000000000000000000
      00003FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000ACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FE000000000000000000000000000000000000000000000000000000000000
      0000621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE000000000000000000000000000000000000000000000000000000000000
      0000897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C5FFE897C
      5FFE000000000000000000000000000000000000000000000000000000000000
      00003FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5FDFE3FD5
      FDFE000000000000000000000000000000000000000000000000000000000000
      0000ACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE68FEACEE
      68FE000000000000000000000000000000000000000000000000000000000000
      0000621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000C020B45771A69D2771A69D2771A69D2771A69D2771A
      69D2771A69D20C020B45000000000000000000000000000000000C020B45771A
      69D2771A69D2771A69D2771A69D2771A69D2771A69D20C020B45000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000702104543149ED243149ED243149ED243149ED24314
      9ED243149ED20702104500000000000000000000000000000000070210454314
      9ED243149ED243149ED243149ED243149ED243149ED207021045000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000D0504457B362AD27B362AD27B362AD27B362AD27B36
      2AD27B362AD20D050445000000000000000000000000000000000D0504457B36
      2AD27B362AD27B362AD27B362AD27B362AD27B362AD20D050445000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000D0407457C2745D27C2745D27C2745D27C2745D27C27
      45D27C2745D20D040745000000000000000000000000000000000D0407457C27
      45D27C2745D27C2745D27C2745D27C2745D27C2745D20D040745000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE1D061A68000000000000000000000000000000001D061A69AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE100426680000000000000000000000000000000010042769621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE1E0D0A68000000000000000000000000000000001E0D0A69B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE1E091168000000000000000000000000000000001E091169B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE1D061A68000000000000000000000000000000001D061A69AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE100426680000000000000000000000000000000010042769621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE1E0D0A68000000000000000000000000000000001E0D0A69B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE1E091168000000000000000000000000000000001E091169B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE1D061A68000000000000000000000000000000001D061A69AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE100426680000000000000000000000000000000010042769621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE1E0D0A68000000000000000000000000000000001E0D0A69B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE1E091168000000000000000000000000000000001E091169B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE1D061A68000000000000000000000000000000001D061A69AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE100426680000000000000000000000000000000010042769621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE1E0D0A68000000000000000000000000000000001E0D0A69B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE1E091168000000000000000000000000000000001E091169B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE1D061A68000000000000000000000000000000001D061A69AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE100426680000000000000000000000000000000010042769621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE1E0D0A68000000000000000000000000000000001E0D0A69B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE1E091168000000000000000000000000000000001E091169B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE1D061A68000000000000000000000000000000001D061A69AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE100426680000000000000000000000000000000010042769621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE1E0D0A68000000000000000000000000000000001E0D0A69B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE1E091168000000000000000000000000000000001E091169B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE1D061A68000000000000000000000000000000001D061A69AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE100426680000000000000000000000000000000010042769621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE1E0D0A68000000000000000000000000000000001E0D0A69B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE1E091168000000000000000000000000000000001E091169B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEA42491F6771A69D2771A69D2771A69D2771A69D2A42491F6AE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE5C1BD9F643149ED243149ED243149ED243149ED25C1BD9F6621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEA94A3AF67B362AD27B362AD27B362AD27B362AD2A94A3AF6B350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEAB355FF67C2745D27C2745D27C2745D27C2745D2AB355FF6B539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D061A69AE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE1D061A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010042769621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE10042668000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E0D0A69B3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE1E0D0A68000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E091169B53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE1E091168000000000000
      00000000000000000000000000000000000000000000000000000000000E8F1F
      7FE6AE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFE8F1F7FE60000000E000000000000000000000000000000000000000E4F18
      BDE6621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE4F18BDE60000000E000000000000000000000000000000000000000E9341
      33E6B3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFE934133E60000000E000000000000000000000000000000000000000E952E
      53E6B53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FE952E53E60000000E00000000000000000000000000000000000000000000
      00066F1862CBAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE6F18
      62CB000000060000000000000000000000000000000000000000000000000000
      00063E1393CB621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE3E13
      93CB000000060000000000000000000000000000000000000000000000000000
      0006723227CBB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE7232
      27CB000000060000000000000000000000000000000000000000000000000000
      0006742441CBB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FE7424
      41CB000000060000000000000000000000000000000000000000000000000000
      000000000000501147ADAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE501147AD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002D0D6BAD621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE2D0D6BAD0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000053251DADB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE53251DAD0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000531A2FADB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FE531A2FAD0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000360C308EAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFE360C308E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E09488E621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE1E09488E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003818138EB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFE3818138E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000038111F8EB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FE38111F8E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000021071E70AE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFE21071E7000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000012052C70621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE12052C7000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000220F0C70B3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFE220F0C7000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000230A1370B53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FE230A137000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000013041155AE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFE130411550000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000A031955621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE0A0319550000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000013080655B3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFE130806550000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000014060B55B53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FEB53966FE14060B550000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A02093EAE269AFEAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFEAE269AFE0A02093E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000005010D3E621DE7FE621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE621DE7FE05010D3E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A04033EB3503EFEB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFEB3503EFE0A04033E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A03063EB53966FEB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FEB53966FE0A03063E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000501042BAE26
      9AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE26
      9AFEAE269AFE0501042B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000200062B621D
      E7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621D
      E7FE621DE7FE0200062B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000502012BB350
      3EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB350
      3EFEB3503EFE0502012B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000501022BB539
      66FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB539
      66FEB53966FE0501022B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000200
      011BAD269AFDAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAE269AFEAD26
      9AFD0200011B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000100
      021B601DE5FD621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE621DE7FE601D
      E5FD0100021B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000200
      001BB24F3EFDB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB3503EFEB24F
      3EFD0200001B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000200
      011BB43964FDB53966FEB53966FEB53966FEB53966FEB53966FEB53966FEB439
      64FD0200011B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000E8F1F7FE6AE269AFEAE269AFEAE269AFEAE269AFE8F1F7FE60000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000E4F18BDE6621DE7FE621DE7FE621DE7FE621DE7FE4F18BDE60000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000E934133E6B3503EFEB3503EFEB3503EFEB3503EFE934133E60000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000E952E53E6B53966FEB53966FEB53966FEB53966FE952E53E60000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000066F1862CBAE269AFEAE269AFE6F1862CB000000060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000063E1393CB621DE7FE621DE7FE3E1393CB000000060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000006723227CBB3503EFEB3503EFE723227CB000000060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000006742441CBB53966FEB53966FE742441CB000000060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000501147AD501147AD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002D0D6BAD2D0D6BAD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000053251DAD53251DAD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000531A2FAD531A2FAD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000A00000000100010000000000000A00000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
end
