object StyledButtonCustomEditor: TStyledButtonCustomEditor
  Left = 0
  Top = 0
  Caption = 'Styled Button Attributes Editor %s - Copyright Ethea S.r.l.'
  ClientHeight = 531
  ClientWidth = 850
  Color = clBtnFace
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
  object BottomPanel: TPanel
    Left = 0
    Top = 493
    Width = 850
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      850
      38)
    object OKButton: TStyledButton
      Left = 553
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
    object ApplyButton: TStyledButton
      Left = 736
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyButtonClick
    end
    object CancelButton: TStyledButton
      Left = 645
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TStyledButton
      Left = 828
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
    Width = 850
    Height = 101
    Align = alTop
    TabOrder = 0
    OnResize = paTopResize
    object ActualGroupBox: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 413
      Height = 93
      Align = alLeft
      Caption = 'ACTUAL'
      TabOrder = 0
      object SourceButton: TStyledGraphicButton
        AlignWithMargins = True
        Left = 8
        Top = 24
        Width = 120
        Height = 34
        StyleElements = [seFont, seBorder]
        Caption = 'Source Button'
        StyleClass = 'Windows10'
      end
    end
    object NewGroupBox: TGroupBox
      AlignWithMargins = True
      Left = 423
      Top = 4
      Width = 508
      Height = 93
      Align = alClient
      Caption = 'NEW'
      TabOrder = 1
      object DestButton: TStyledGraphicButton
        AlignWithMargins = True
        Left = 12
        Top = 24
        Width = 120
        Height = 34
        OnClick = DestButtonClick
        StyleElements = [seFont, seBorder]
        Caption = 'Dest Button'
        StyleClass = 'Windows10'
      end
    end
  end
  object AttributesPanel: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 104
    Width = 844
    Height = 59
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object AttributesGroupBox: TGroupBox
      Left = 0
      Top = 0
      Width = 929
      Height = 59
      Align = alClient
      Caption = 'Button Attributes'
      TabOrder = 0
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
        Width = 225
        Height = 32
        Max = 30
        Min = 1
        Position = 1
        TabOrder = 2
        OnChange = RadiusTrackBarChange
      end
      object FlatButtonCheckBox: TCheckBox
        Left = 695
        Top = 25
        Width = 106
        Height = 17
        Caption = 'Flat button'
        TabOrder = 3
        OnClick = FlatButtonCheckBoxClick
      end
    end
  end
  object TabControl: TTabControl
    Left = 0
    Top = 166
    Width = 850
    Height = 327
    Align = alClient
    TabHeight = 32
    TabOrder = 3
    Tabs.Strings = (
      'Normal'
      'Pressed'
      'Selected'
      'Hot'
      'Disabled')
    TabIndex = 0
    object ScrollBox: TScrollBox
      AlignWithMargins = True
      Left = 7
      Top = 41
      Width = 836
      Height = 279
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsNone
      ParentBackground = True
      TabOrder = 0
    end
  end
end
