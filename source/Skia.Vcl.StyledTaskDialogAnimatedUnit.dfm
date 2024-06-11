inherited StyledTaskDialogAnimatedForm: TStyledTaskDialogAnimatedForm
  Caption = 'StyledTaskDialogAnimatedForm'
  TextHeight = 15
  inherited FooterPanel: TPanel
    inherited FooterIconPanel: TPanel
      object SkFooterAnimatedImage: TSkAnimatedImage
        Left = 0
        Top = 0
        Width = 25
        Height = 25
        Align = alTop
      end
    end
  end
  inherited CenterPanel: TPanel
    inherited ImagePanel: TPanel
      inherited IconContainer: TPanel
        object SkAnimatedImage: TSkAnimatedImage
          Left = 0
          Top = 0
          Width = 64
          Height = 64
          Align = alClient
        end
      end
    end
  end
end
