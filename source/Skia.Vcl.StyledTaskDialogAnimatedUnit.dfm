inherited StyledTaskDialogAnimated: TStyledTaskDialogAnimated
  Caption = 'StyledTaskDialogAnimated'
  TextHeight = 13
  inherited CenterPanel: TPanel
    inherited ImagePanel: TPanel
      object SkAnimatedImage: TSkAnimatedImage
        Left = 0
        Top = 0
        Width = 128
        Height = 128
        Align = alTop
        ExplicitLeft = 1
        ExplicitTop = 2
      end
    end
  end
end
