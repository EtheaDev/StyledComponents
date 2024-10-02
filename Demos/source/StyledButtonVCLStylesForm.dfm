object fmStyledButtonVCLStyles: TfmStyledButtonVCLStyles
  Left = 0
  Top = 0
  Caption = 'Styled Buttons with VCL Styles'
  ClientHeight = 681
  ClientWidth = 988
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 988
    Height = 65
    Align = alTop
    Color = clGray
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    object RenderRadioGroup: TRadioGroup
      AlignWithMargins = True
      Left = 490
      Top = 4
      Width = 346
      Height = 57
      Align = alLeft
      Caption = 'Styled Button Rendering Options'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'As VCL'
        'Rounded'
        'RoundRect'
        'Rectangle'
        'FAB')
      TabOrder = 0
      OnClick = RadioGroupClick
    end
    object StyleRadioGroup: TRadioGroup
      AlignWithMargins = True
      Left = 143
      Top = 4
      Width = 341
      Height = 57
      Align = alLeft
      Caption = 'Styled Button Style'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Push Button'
        'Split Button'
        'Command Link')
      TabOrder = 1
      OnClick = RadioGroupClick
    end
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 133
      Height = 57
      Align = alLeft
      Caption = 'Buttons Attributes'
      TabOrder = 2
      object EnabledCheckBox: TCheckBox
        Left = 17
        Top = 15
        Width = 100
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = EnabledCheckBoxClick
      end
      object OutlineCheckBox: TCheckBox
        Left = 17
        Top = 32
        Width = 100
        Height = 17
        Caption = 'Outline'
        TabOrder = 1
        OnClick = EnabledCheckBoxClick
      end
    end
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 65
    Width = 489
    Height = 616
    Align = alLeft
    TabOrder = 1
    object LeftScrollBox: TScrollBox
      Left = 1
      Top = 25
      Width = 487
      Height = 590
      Align = alClient
      TabOrder = 0
      OnMouseWheel = ScrollBoxMouseWheel
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 487
      Height = 24
      Align = alTop
      Caption = 'VCL TButtons with StyleName="VCLStyle"'
      TabOrder = 1
    end
  end
  object RightPanel: TPanel
    Left = 489
    Top = 65
    Width = 499
    Height = 616
    Align = alClient
    TabOrder = 2
    object RightScrollBox: TScrollBox
      Left = 1
      Top = 25
      Width = 497
      Height = 590
      Align = alClient
      TabOrder = 0
      OnMouseWheel = ScrollBoxMouseWheel
    end
    object TopRightPanel: TPanel
      Left = 1
      Top = 1
      Width = 497
      Height = 24
      Align = alTop
      Caption = 
        'TStyledButtons with StyleFamily=Classic and StyleClass="VCLStyle' +
        '"'
      TabOrder = 1
    end
  end
  object PopupMenu: TPopupMenu
    Left = 782
    Top = 259
    object New1: TMenuItem
      Caption = '&New'
      OnClick = PopUpMenuClick
    end
    object Open1: TMenuItem
      Caption = '&Open...'
      OnClick = PopUpMenuClick
    end
    object Save1: TMenuItem
      Caption = '&Save'
      OnClick = PopUpMenuClick
    end
    object SaveAs1: TMenuItem
      Caption = 'Save &As...'
      OnClick = PopUpMenuClick
    end
    object Exit1: TMenuItem
      Caption = 'E&xit'
      OnClick = PopUpMenuClick
    end
  end
end
