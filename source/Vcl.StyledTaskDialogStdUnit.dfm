inherited StyledTaskDialogStd: TStyledTaskDialogStd
  Caption = 'StyledTaskDialogStd'
  TextHeight = 15
  inherited FooterPanel: TPanel
    inherited FooterIconPanel: TPanel
      object FooterImage: TImage
        Left = 0
        Top = 0
        Width = 25
        Height = 25
        Align = alTop
        Stretch = True
        Transparent = True
      end
    end
  end
  inherited CenterPanel: TPanel
    inherited ImagePanel: TPanel
      inherited IconContainer: TPanel
        DoubleBuffered = True
        ParentDoubleBuffered = False
        object Image: TImage
          Left = 0
          Top = 0
          Width = 64
          Height = 64
          Align = alClient
          Stretch = True
          Transparent = True
        end
      end
    end
  end
end
