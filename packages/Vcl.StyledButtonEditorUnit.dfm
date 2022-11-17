object StyledButtonEditor: TStyledButtonEditor
  Left = 0
  Top = 0
  ActiveControl = TabControl
  Caption = 'Styled Button Editor %s - Copyright Ethea S.r.l.'
  ClientHeight = 542
  ClientWidth = 872
  Color = clWindow
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  DoubleBuffered = True
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
    Top = 162
    Width = 872
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 56
    ExplicitWidth = 519
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 504
    Width = 872
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      872
      38)
    object OKButton: TButton
      Left = 483
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
      Left = 666
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyButtonClick
    end
    object CancelButton: TButton
      Left = 575
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
      Left = 758
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
    Width = 872
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
        Left = 11
        Top = 28
        Width = 120
        Height = 34
        Caption = 'Source Button'
      end
    end
    object NewGroupBox: TGroupBox
      Left = 176
      Top = 1
      Width = 695
      Height = 99
      Align = alClient
      Caption = 'New appearance'
      TabOrder = 1
      object DestButton: TStyledGraphicButton
        Tag = 0
        AlignWithMargins = True
        Left = 12
        Top = 28
        Width = 120
        Height = 34
        OnClick = DestButtonClick
        Caption = 'Dest Button'
      end
    end
  end
  object TabControl: TTabControl
    Left = 0
    Top = 165
    Width = 872
    Height = 339
    Align = alClient
    TabHeight = 32
    TabOrder = 0
    Tabs.Strings = (
      'Classic')
    TabIndex = 0
    OnChange = TabControlChange
    object ClassesGroupBox: TGroupBox
      AlignWithMargins = True
      Left = 7
      Top = 41
      Width = 858
      Height = 291
      Align = alClient
      Caption = 'Classes'
      TabOrder = 0
      object ScrollBox: TScrollBox
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 848
        Height = 268
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 0
      end
    end
  end
  object AttributesGroupBox: TGroupBox
    Left = 0
    Top = 101
    Width = 872
    Height = 61
    Align = alTop
    Caption = 'Button Attributes'
    TabOrder = 3
    object StyleDrawTypeLabel: TLabel
      Left = 19
      Top = 26
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'StyleDrawType:'
    end
    object StyleRadiusLabel: TLabel
      Left = 386
      Top = 26
      Width = 69
      Height = 13
      Alignment = taRightJustify
      Caption = 'StyledRadius:'
    end
    object EnabledCheckBox: TCheckBox
      Left = 253
      Top = 25
      Width = 106
      Height = 17
      Caption = 'Button Enabled'
      TabOrder = 1
      OnClick = EnabledCheckBoxClick
    end
    object StyleDrawTypeComboBox: TComboBox
      Left = 102
      Top = 23
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnSelect = StyleDrawTypeComboBoxSelect
    end
    object RadiusTrackBar: TTrackBar
      Left = 464
      Top = 20
      Width = 192
      Height = 32
      Max = 25
      Min = 1
      Position = 1
      TabOrder = 2
      OnChange = RadiusTrackBarChange
    end
  end
end
