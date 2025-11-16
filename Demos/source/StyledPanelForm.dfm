object fmStyledPanel: TfmStyledPanel
  Left = 0
  Top = 0
  Caption = 'Styled Panel - Family/Class/Appearance'
  ClientHeight = 691
  ClientWidth = 895
  Color = clWindow
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 895
    Height = 691
    ActivePage = tsBootstrap
    Align = alClient
    Images = VirtualImageList32
    TabOrder = 0
    object tsBootstrap: TTabSheet
      Caption = 'Bootstrap'
      ImageIndex = 20
      ImageName = 'Bootstrap-logo'
      object BootstrapScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 887
        Height = 646
        Align = alClient
        TabOrder = 0
        OnMouseWheel = ScrollBoxMouseWheel
        ExplicitHeight = 663
        object BootstrapNormalGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Normal Bootstrap Panels'
          TabOrder = 0
          object BootstrapNormalFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object BootstrapOutlineGroupBox: TGroupBox
          Left = 0
          Top = 140
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Outline Bootstrap Panels'
          TabOrder = 1
          object BootstrapOutlineFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
    end
    object tsAngular: TTabSheet
      Caption = 'Angular'
      ImageIndex = 19
      ImageName = 'Angular-logo'
      object AngularScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 887
        Height = 646
        Align = alClient
        TabOrder = 0
        OnMouseWheel = ScrollBoxMouseWheel
        ExplicitHeight = 663
        object AngularBasicGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Basic Angular Panels'
          TabOrder = 2
          object AngularBasicFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object AngularRaisedGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Raised Angular Panels'
          TabOrder = 0
          object AngularRaisedFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object AngularStrokedGroupBox: TGroupBox
          Left = 0
          Top = 280
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Stroked Angular Panels'
          TabOrder = 1
          object AngularStrokedFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object AngularFlatGroupBox: TGroupBox
          Left = 0
          Top = 420
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Flat Angular Panels'
          TabOrder = 3
          ExplicitTop = 500
          object AngularFlatFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
    end
    object tsClassic: TTabSheet
      Caption = 'Classic'
      ImageIndex = 21
      ImageName = 'Delphi-Logo'
      object ClassicScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 887
        Height = 646
        Align = alClient
        TabOrder = 0
        OnMouseWheel = ScrollBoxMouseWheel
        ExplicitHeight = 663
        object ClassicNormalGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Normal Classic Panels (similar to VCL Styled)'
          TabOrder = 0
          object ClassicNormalFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object ClassicOutlineGroupBox: TGroupBox
          Left = 0
          Top = 140
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Outline Classic Panels'
          TabOrder = 1
          object ClassicOutlineFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
    end
    object tsBasicColor: TTabSheet
      Caption = 'Basic-Color'
      ImageIndex = 22
      ImageName = 'Basic-Color'
      object BasicColorScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 887
        Height = 646
        Align = alClient
        TabOrder = 0
        OnMouseWheel = ScrollBoxMouseWheel
        ExplicitHeight = 663
        object BasicColorNormalGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Normal Basic-Color Panels'
          TabOrder = 0
          object BasicColorNormalFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object BasicColorOutlineGroupBox: TGroupBox
          Left = 0
          Top = 140
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Outline Basic-Color Panels'
          TabOrder = 1
          object BasicColorOutlineFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
    end
    object tsSVGColor: TTabSheet
      Caption = 'SVG-Color'
      ImageIndex = 23
      ImageName = 'SVG-Color'
      object SvgColorScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 887
        Height = 646
        Align = alClient
        TabOrder = 0
        OnMouseWheel = ScrollBoxMouseWheel
        ExplicitHeight = 663
        object SvgColorNormalGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Normal SVG-Color Panels'
          TabOrder = 0
          object SvgColorNormalFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
        object SvgColorOutlineGroupBox: TGroupBox
          Left = 0
          Top = 140
          Width = 883
          Height = 140
          Align = alTop
          Caption = 'Outline SVG-Color Panels'
          TabOrder = 1
          object SvgColorOutlineFlowPanel: TFlowPanel
            Left = 2
            Top = 15
            Width = 879
            Height = 123
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            OnResize = FlowPanelResize
          end
        end
      end
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
    Left = 460
    Top = 542
  end
end
