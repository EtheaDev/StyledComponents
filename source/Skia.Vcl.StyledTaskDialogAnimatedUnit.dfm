inherited StyledTaskDialogAnimated: TStyledTaskDialogAnimated
  Caption = 'StyledTaskDialogAnimated'
  ExplicitWidth = 608
  ExplicitHeight = 283
  TextHeight = 13
  inherited BottomBevel: TBevel
    Top = 144
  end
  inherited FooterPanel: TPanel
    ExplicitTop = 187
    ExplicitWidth = 592
  end
  inherited CenterPanel: TPanel
    ExplicitWidth = 584
    ExplicitHeight = 136
    inherited ImagePanel: TPanel
      ExplicitTop = 5
      ExplicitHeight = 128
      object SkAnimatedImage: TSkAnimatedImage
        Left = 0
        Top = 0
        Width = 128
        Height = 128
        Align = alClient
        ExplicitLeft = 78
        ExplicitTop = 78
        ExplicitWidth = 50
        ExplicitHeight = 50
      end
    end
    inherited MessageScrollBox: TScrollBox
      ExplicitWidth = 448
      ExplicitHeight = 136
    end
  end
  inherited ButtonsPanel: TPanel
    Top = 147
    ExplicitTop = 147
    ExplicitWidth = 592
  end
end
