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
    DesignSize = (
      988
      57)
    object StyleLabel: TLabel
      Left = 11
      Top = 5
      Width = 130
      Height = 13
      Caption = 'Change application style:'
    end
    object ShowEditButton: TStyledButton
      Left = 191
      Top = 11
      Width = 90
      Height = 30
      Hint = 'Show Styled Button Editor'
      Action = TestAction
      ImageName = 'approval'
      ModalResult = 12
      TabOrder = 1
    end
    object cbChangeStyle: TComboBox
      Left = 11
      Top = 20
      Width = 174
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnSelect = cbChangeStyleSelect
    end
    object AboutButton: TButton
      Left = 287
      Top = 11
      Width = 88
      Height = 30
      Caption = 'About'
      TabOrder = 2
      OnClick = AboutButtonClick
    end
    object RenderRadioGroup: TRadioGroup
      Left = 536
      Top = 5
      Width = 405
      Height = 42
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Styled Button Rendering Options'
      Columns = 4
      ItemIndex = 0
      Items.Strings = (
        'Same as VCL'
        'Rounded'
        'Rectangle'
        'FAB')
      TabOrder = 4
      OnClick = RenderRadioGroupClick
      ExplicitWidth = 401
    end
    object SplitButtonsCheckBox: TCheckBox
      Left = 408
      Top = 4
      Width = 97
      Height = 17
      Caption = 'Split Buttons'
      TabOrder = 3
      OnClick = SplitButtonsCheckBoxClick
    end
    object EnabledCheckBox: TCheckBox
      Left = 408
      Top = 27
      Width = 97
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = EnabledCheckBoxClick
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
    Left = 785
    Top = 211
    object TestAction: TAction
      Caption = 'Show Editor'
      Hint = 'Hint of Action'
      ImageName = 'approval'
      OnExecute = TestActionExecute
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
    Left = 780
    Top = 326
  end
end
