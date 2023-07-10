object TestMainForm: TTestMainForm
  Left = 0
  Top = 0
  Caption = 'Styled Button Test (c) Copyright Ethea S.r.l.'
  ClientHeight = 641
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
    Height = 57
    Align = alTop
    Color = clGray
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
    ExplicitWidth = 984
    object StyleLabel: TLabel
      Left = 11
      Top = 5
      Width = 130
      Height = 13
      Caption = 'Change application style:'
    end
    object ShowEditButton: TStyledButton
      Left = 335
      Top = 12
      Width = 90
      Height = 30
      Action = TestAction
      ModalResult = 12
      TabOrder = 1
    end
    object cbChangeStyle: TComboBox
      Left = 11
      Top = 20
      Width = 302
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnSelect = cbChangeStyleSelect
    end
    object VCLButton: TButton
      Left = 431
      Top = 12
      Width = 88
      Height = 30
      Caption = 'VCL Button'
      TabOrder = 2
    end
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 57
    Width = 489
    Height = 584
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 583
    object LeftScrollBox: TScrollBox
      Left = 1
      Top = 25
      Width = 487
      Height = 558
      Align = alClient
      TabOrder = 0
      OnMouseWheel = ScrollBoxMouseWheel
      ExplicitHeight = 557
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
    Top = 57
    Width = 499
    Height = 584
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 495
    ExplicitHeight = 583
    object RightScrollBox: TScrollBox
      Left = 1
      Top = 25
      Width = 497
      Height = 558
      Align = alClient
      TabOrder = 0
      OnMouseWheel = ScrollBoxMouseWheel
      ExplicitWidth = 493
      ExplicitHeight = 557
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
      ExplicitWidth = 493
    end
  end
  object ActionList: TActionList
    Left = 929
    Top = 11
    object TestAction: TAction
      Caption = 'Show Editor'
      Hint = 'Hint of Action'
      ImageName = 'approval'
      OnExecute = TestActionExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 990
    Top = 11
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
