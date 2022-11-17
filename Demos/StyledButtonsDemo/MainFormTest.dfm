object TestMainForm: TTestMainForm
  Left = 0
  Top = 0
  Caption = 'Styled Button Test (c) Copyright Ethea S.r.l.'
  ClientHeight = 105
  ClientWidth = 836
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 836
    Height = 70
    Align = alTop
    Color = clGray
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    ExplicitWidth = 832
    object ShowEditButton: TStyledButton
      Tag = 0
      Left = 207
      Top = 14
      Width = 172
      Height = 40
      Action = TestAction
      PopUpMenu = PopupMenu
      ModalResult = 9
      StyleFamily = 'Bootstrap'
      StyleClass = 'Primary'
      StyleAppearance = 'Normal'
      TabOrder = 1
    end
    object StyleLabel: TLabel
      Left = 11
      Top = 14
      Width = 130
      Height = 13
      Caption = 'Change application style:'
    end
    object cbChangeStyle: TComboBox
      Left = 11
      Top = 33
      Width = 190
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnSelect = cbChangeStyleSelect
    end
    object Button1: TButton
      Left = 458
      Top = 14
      Width = 173
      Height = 40
      Caption = 'Button1'
      TabOrder = 2
    end
  end
  object ActionList: TActionList
    Left = 329
    Top = 19
    object TestAction: TAction
      Caption = 'Show Editor'
      Hint = 'Hint of Action'
      ImageName = 'approval'
      OnExecute = TestActionExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 414
    Top = 19
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
end
