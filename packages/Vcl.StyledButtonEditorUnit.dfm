object StyledButtonEditor: TStyledButtonEditor
  Left = 0
  Top = 0
  ActiveControl = TabControl
  Caption = 'Styled Button Editor %s - Copyright Ethea S.r.l.'
  ClientHeight = 459
  ClientWidth = 880
  Color = clWindow
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object SplitterTop: TSplitter
    Left = 0
    Top = 101
    Width = 880
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 56
    ExplicitWidth = 519
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 421
    Width = 880
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      880
      38)
    object OKButton: TButton
      Left = 499
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OKButtonClick
    end
    object ApplyButton: TButton
      Left = 682
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyButtonClick
    end
    object CancelButton: TButton
      Left = 591
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 774
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 880
    Height = 101
    Align = alTop
    TabOrder = 1
    OnResize = paTopResize
    object ActualGroupBox: TGroupBox
      Left = 1
      Top = 1
      Width = 175
      Height = 99
      Align = alLeft
      Caption = 'Actual Appearance'
      TabOrder = 0
      object SourceButton: TStyledGraphicButton
        Tag = 0
        AlignWithMargins = True
        Left = 12
        Top = 24
        Width = 120
        Height = 34
        Caption = 'Source Button'
      end
    end
    object NewGroupBox: TGroupBox
      Left = 176
      Top = 1
      Width = 703
      Height = 99
      Align = alClient
      Caption = 'New appearance'
      TabOrder = 1
      object DestButton: TStyledGraphicButton
        Tag = 0
        AlignWithMargins = True
        Left = 12
        Top = 24
        Width = 120
        Height = 34
        Caption = 'Dest Button'
      end
    end
  end
  object TabControl: TTabControl
    Left = 0
    Top = 104
    Width = 880
    Height = 317
    Align = alClient
    TabHeight = 32
    TabOrder = 0
    Tabs.Strings = (
      'Classic')
    TabIndex = 0
    OnChange = TabControlChange
    object ClassesGroupBox: TGroupBox
      Left = 4
      Top = 38
      Width = 872
      Height = 275
      Align = alClient
      Caption = 'Classes'
      TabOrder = 0
      object ScrollBox: TScrollBox
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 862
        Height = 252
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
  end
end
