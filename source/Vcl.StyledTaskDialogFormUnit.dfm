object StyledTaskDialogForm: TStyledTaskDialogForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSizeToolWin
  ClientHeight = 244
  ClientWidth = 592
  Color = clWindow
  Constraints.MinHeight = 200
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 184
    Width = 592
    Height = 3
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 200
    ExplicitWidth = 982
  end
  object FooterPanel: TPanel
    Left = 0
    Top = 187
    Width = 592
    Height = 57
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 175
    ExplicitWidth = 584
    object FooterTextLabel: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 81
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = 'FooterTextLabel'
      WordWrap = True
    end
  end
  object CenterPanel: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 584
    Height = 136
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 576
    ExplicitHeight = 124
    object ImagePanel: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 128
      Height = 128
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 116
    end
    object MessageScrollBox: TScrollBox
      Left = 136
      Top = 0
      Width = 448
      Height = 136
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      ExplicitWidth = 440
      ExplicitHeight = 124
      object TitleLabel: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 45
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'Title Text'
        WordWrap = True
      end
      object AutoSizeLabel: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 25
        Width = 72
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'AutoSizeLabel'
        WordWrap = True
      end
      object TextLabel: TLinkLabel
        AlignWithMargins = True
        Left = 4
        Top = 46
        Width = 830
        Height = 15
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        BevelInner = bvNone
        BevelOuter = bvNone
        Caption = 'Text Label'
        TabOrder = 0
        OnLinkClick = TextLabelLinkClick
      end
    end
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 144
    Width = 592
    Height = 40
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    ExplicitTop = 132
    ExplicitWidth = 584
    object YesButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = -377
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Yes'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Primary'
      TabOrder = 0
      ExplicitLeft = 5
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object NoButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = -296
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'No'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Secondary'
      TabOrder = 1
      ExplicitLeft = 86
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object OKButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = -215
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'OK'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Success'
      TabOrder = 2
      ExplicitLeft = 167
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object CancelButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = -134
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Cancel'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Danger'
      TabOrder = 3
      ExplicitLeft = 248
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object AbortButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = -53
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Abort'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Danger'
      TabOrder = 4
      ExplicitLeft = 329
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object RetryButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = 28
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Retry'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Warning'
      TabOrder = 5
      ExplicitLeft = 410
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object IgnoreButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = 109
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      OnClick = ButtonClick
      ParentFont = False
      Visible = False
      Caption = 'Ignore'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Secondary'
      TabOrder = 6
      ExplicitLeft = 491
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object AllButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = 190
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      OnClick = ButtonClick
      ParentFont = False
      Visible = False
      Caption = 'All'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Primary'
      StyleAppearance = 'Outline'
      TabOrder = 7
      ExplicitLeft = 572
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object NoToAllButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = 271
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      OnClick = ButtonClick
      ParentFont = False
      Visible = False
      Caption = 'No to All'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Danger'
      StyleAppearance = 'Outline'
      TabOrder = 8
      ExplicitLeft = 653
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object YesToAllButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = 352
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      OnClick = ButtonClick
      ParentFont = False
      Visible = False
      Caption = 'Yes to All'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Success'
      StyleAppearance = 'Outline'
      TabOrder = 9
      ExplicitLeft = 734
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object HelpButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = 433
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      OnClick = HelpButtonClick
      ParentFont = False
      Visible = False
      Caption = 'Help'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Info'
      TabOrder = 10
      ExplicitLeft = 815
      ExplicitTop = 3
      ExplicitHeight = 30
    end
    object CloseButton: TStyledButton
      Tag = 0
      AlignWithMargins = True
      Left = 514
      Top = 4
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      OnClick = ButtonClick
      ParentFont = False
      Visible = False
      Caption = 'Close'
      StyleFamily = 'Bootstrap'
      StyleClass = 'Secondary'
      StyleAppearance = 'Outline'
      TabOrder = 11
      ExplicitLeft = 896
      ExplicitTop = 3
      ExplicitHeight = 30
    end
  end
end
