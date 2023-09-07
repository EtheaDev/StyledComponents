object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'StyledToolbar Test'
  ClientHeight = 534
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnClick = ToolButtonclick
  OnCreate = FormCreate
  TextHeight = 15
  object ToolBar1: TToolBar
    AlignWithMargins = True
    Left = 3
    Top = 69
    Width = 735
    Height = 66
    AutoSize = True
    ButtonHeight = 66
    ButtonWidth = 55
    Caption = 'ToolBar Caption'
    Images = VirtualImageList
    ShowCaptions = True
    TabOrder = 1
    TabStop = True
    OnClick = ToolBarClick
    OnMouseEnter = ToolBar1MouseEnter
    ExplicitWidth = 731
    object ToolButton2: TToolButton
      Left = 0
      Top = 0
      Hint = 'Open Button'
      AllowAllUp = True
      Caption = 'Open'
      Down = True
      Grouped = True
      ImageIndex = 12
      ImageName = 'launch-black'
      Style = tbsCheck
      OnClick = ToolButtonclick
    end
    object ToolButton1: TToolButton
      Left = 55
      Top = 0
      Hint = 'Menu button'
      Caption = 'Home'
      Grouped = True
      ImageIndex = 2
      ImageName = 'home-pink'
      Style = tbsCheck
      OnClick = ToolButtonclick
    end
    object ToolButton5: TToolButton
      Left = 110
      Top = 0
      Width = 10
      Hint = 'Hint of Sep'
      Caption = 'Sep'
      ImageIndex = 11
      ImageName = 'launch-white'
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 120
      Top = 0
      Hint = 'Like Button'
      Caption = 'Like'
      Grouped = True
      ImageIndex = 8
      ImageName = 'heart'
      Style = tbsCheck
      OnClick = ToolButtonclick
    end
    object ToolButton4: TToolButton
      Left = 175
      Top = 0
      Hint = 'Options button'
      Caption = 'Options'
      Grouped = True
      ImageIndex = 10
      ImageName = 'dots-vertical-black'
      Style = tbsCheck
      OnClick = ToolButtonclick
    end
    object ToolButton6: TToolButton
      Left = 230
      Top = 0
      Width = 10
      Caption = 'ToolButton6'
      ImageIndex = 9
      ImageName = 'dots-vertical-white'
      Style = tbsDivider
    end
    object ToolButton7: TToolButton
      Left = 240
      Top = 0
      Caption = 'Menu'
      DropdownMenu = PopupMenu
      ImageIndex = 4
      ImageName = 'menu-amber'
      Style = tbsDropDown
    end
    object Edit1: TEdit
      Left = 310
      Top = 0
      Width = 121
      Height = 66
      TabOrder = 0
      Text = 'Edit1'
    end
  end
  object StyledToolbar1: TStyledToolbar
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 735
    Height = 60
    AutoSize = True
    ButtonHeight = 60
    ButtonWidth = 60
    Caption = 'StyledToolBar Caption'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Images = VirtualImageList
    Indent = 3
    ParentFont = False
    ShowCaptions = True
    TabOrder = 0
    OnClick = ToolBarClick
    OnMouseEnter = StyledToolbar1MouseEnter
    StyleRadius = 30
    StyleDrawType = btRounded
    ExplicitLeft = -2
    object StyledToolButton1: TStyledToolButton
      Left = 0
      Top = 0
      OnClick = ToolButtonclick
      Caption = 'Open'
      ImageIndex = 11
      ImageName = 'launch-white'
      Style = tbsCheck
      StyleFamily = 'Bootstrap'
      StyleClass = 'Success'
      AllowAllUp = True
      Down = True
      Grouped = True
    end
    object StyledToolButton2: TStyledToolButton
      Left = 60
      Top = 0
      OnClick = ToolButtonclick
      Caption = 'Home'
      ImageIndex = 2
      ImageName = 'home-pink'
      Style = tbsCheck
      StyleFamily = 'Angular-Light'
      StyleClass = 'Amber'
      StyleAppearance = 'Flat'
      Grouped = True
    end
    object SSep1: TStyledToolButton
      Left = 120
      Top = 0
      Style = tbsSeparator
    end
    object StyledToolButton3: TStyledToolButton
      Left = 128
      Top = 0
      OnClick = ToolButtonclick
      Caption = 'Like'
      ImageIndex = 8
      ImageName = 'heart'
      Style = tbsCheck
      Down = True
      Grouped = True
    end
    object StyledToolButton5: TStyledToolButton
      Left = 188
      Top = 0
      OnClick = ToolButtonclick
      StyleElements = [seFont, seBorder]
      Caption = 'Options'
      ImageIndex = 9
      ImageName = 'dots-vertical-white'
      Style = tbsCheck
      StyleClass = 'Aqua Graphite'
      Grouped = True
    end
    object StyledToolButton6: TStyledToolButton
      Left = 320
      Top = 0
      Style = tbsSeparator
    end
    object StyledToolButton4: TStyledToolButton
      Left = 248
      Top = 0
      Caption = 'Menu'
      DropDownMenu = PopupMenu
      ImageIndex = 4
      ImageName = 'menu-amber'
      Style = tbsDropDown
    end
    object Edit2: TEdit
      AlignWithMargins = True
      Left = 331
      Top = 3
      Width = 103
      Height = 23
      TabOrder = 0
      Text = 'Edit2'
    end
    object StyledToolButton7: TStyledToolButton
      Left = 437
      Top = 0
      Caption = 'StyledToolButton7'
      ImageIndex = 0
      ImageName = 'home-deeppurple'
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 423
    Width = 741
    Height = 78
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 422
    ExplicitWidth = 737
    object LeftPanel: TPanel
      Left = 1
      Top = 1
      Width = 122
      Height = 76
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object CreateButton: TStyledButton
        Left = 8
        Top = 14
        Width = 108
        Height = 41
        Hint = 'Create Toolbar "runtime"'
        OnClick = CreateButtonClick
        Caption = 'Create Toolbars'
        TabOrder = 0
      end
    end
    object BottomClientPanel: TPanel
      Left = 123
      Top = 1
      Width = 617
      Height = 76
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      ExplicitWidth = 613
      object WidthLabel: TLabel
        Left = 20
        Top = 15
        Width = 35
        Height = 15
        Alignment = taRightJustify
        Caption = 'Width:'
      end
      object HeightLabel: TLabel
        Left = 16
        Top = 39
        Width = 39
        Height = 15
        Alignment = taRightJustify
        Caption = 'Height:'
      end
      object tbWidth: TTrackBar
        Left = 57
        Top = 5
        Width = 191
        Height = 30
        Hint = 'Change Toolbar.ButtonWidth'
        Max = 100
        Frequency = 5
        Position = 34
        TabOrder = 0
        OnChange = UpdateToolbars
      end
      object tbHeight: TTrackBar
        Left = 57
        Top = 36
        Width = 191
        Height = 30
        Hint = 'Change Toolbar.ButtonHeight'
        Max = 100
        Frequency = 5
        Position = 40
        TabOrder = 1
        OnChange = UpdateToolbars
      end
      object ShowCaptionCheckBox: TCheckBox
        Left = 266
        Top = 13
        Width = 110
        Height = 17
        Caption = 'Show Captions'
        TabOrder = 2
        OnClick = UpdateToolbars
      end
      object ListCheckBox: TCheckBox
        Left = 266
        Top = 36
        Width = 110
        Height = 17
        Caption = 'List'
        TabOrder = 3
        OnClick = UpdateToolbars
      end
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 138
    Width = 741
    Height = 285
    Align = alClient
    TabOrder = 3
    ExplicitWidth = 737
    ExplicitHeight = 284
  end
  object Panel1: TPanel
    Left = 0
    Top = 501
    Width = 741
    Height = 33
    Align = alBottom
    TabOrder = 4
    ExplicitTop = 500
    ExplicitWidth = 737
    object StyleLabel: TLabel
      Left = 11
      Top = 9
      Width = 133
      Height = 15
      Caption = 'Change application style:'
    end
    object cbChangeStyle: TComboBox
      Left = 150
      Top = 6
      Width = 174
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnSelect = cbChangeStyleSelect
    end
  end
  object PopupMenu: TPopupMenu
    Left = 525
    Top = 275
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
  object VirtualImageList: TVirtualImageList
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
      end>
    ImageCollection = dmResources.ImageCollection
    Width = 36
    Height = 36
    Left = 520
    Top = 392
  end
end
