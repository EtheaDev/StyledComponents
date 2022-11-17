inherited StyledTaskDialogAnimated: TStyledTaskDialogAnimated
  Caption = 'StyledTaskDialogAnimated'
  TextHeight = 13
  inherited BottomBevel: TBevel
    Top = 132
    ExplicitTop = 144
  end
  inherited CenterPanel: TPanel
    inherited ImagePanel: TPanel
      object SkAnimatedImage: TSkAnimatedImage
        Left = 0
        Top = 0
        Width = 128
        Height = 128
        Align = alClient
      end
    end
  end
  inherited ButtonsPanel: TPanel
    Top = 135
    ExplicitTop = 147
  end
end
