object StyledTaskDialogForm: TStyledTaskDialogForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSizeToolWin
  ClientHeight = 320
  ClientWidth = 577
  Color = clWindow
  Constraints.MinHeight = 200
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 216
    Width = 577
    Height = 3
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 200
    ExplicitWidth = 301
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 263
    Width = 577
    Height = 57
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 251
    ExplicitWidth = 569
  end
  object CenterPanel: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 569
    Height = 208
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 561
    ExplicitHeight = 196
    object ImagePanel: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 128
      Height = 200
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 188
      object SVGIconImage: TSVGIconImage
        Left = 0
        Top = 0
        Width = 128
        Height = 128
        AutoSize = False
        ImageList = SVGIconImageList
        SVGText = '<svg xmlns="http://www.w3.org/2000/svg"></svg>'
        Align = alTop
      end
    end
    object MessageScrollBox: TScrollBox
      Left = 136
      Top = 0
      Width = 433
      Height = 208
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      ExplicitWidth = 425
      ExplicitHeight = 196
      object TitleLabel: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 425
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'Title Text'
        WordWrap = True
        ExplicitWidth = 45
      end
      object AutoSizeLabel: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 48
        Width = 425
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'AutoSizeLabel'
        WordWrap = True
        ExplicitWidth = 72
      end
      object TextLabel: TLinkLabel
        AlignWithMargins = True
        Left = 4
        Top = 25
        Width = 425
        Height = 15
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        AutoSize = False
        BevelInner = bvNone
        BevelOuter = bvNone
        Caption = 'Text Label'
        TabOrder = 0
        OnLinkClick = TextLabelLinkClick
      end
    end
  end
  object ButtonsPanel: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 223
    Width = 569
    Height = 36
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 211
    ExplicitWidth = 561
    object OKButton: TStyledButton
      AlignWithMargins = True
      Left = 5
      Top = 3
      Height = 30
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'OK'
      TabOrder = 0
      StyleClass = 'Primary'
      ExplicitLeft = -8
      ExplicitTop = 0
    end
    object CancelButton: TStyledButton
      AlignWithMargins = True
      Left = 248
      Top = 3
      Height = 30
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Cancel'
      TabOrder = 3
      StyleClass = 'Secondary'
      ExplicitLeft = 215
      ExplicitTop = 5
    end
    object YesButton: TStyledButton
      AlignWithMargins = True
      Left = 86
      Top = 3
      Height = 30
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'YES'
      TabOrder = 1
      StyleClass = 'Success'
      ExplicitLeft = 33
    end
    object NoButton: TStyledButton
      AlignWithMargins = True
      Left = 167
      Top = 3
      Height = 30
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'NO'
      TabOrder = 2
      StyleClass = 'Danger'
      ExplicitLeft = 124
    end
    object RetryButton: TStyledButton
      AlignWithMargins = True
      Left = 329
      Top = 3
      Height = 30
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Retry'
      TabOrder = 4
      StyleClass = 'Warning'
      ExplicitLeft = 196
      ExplicitTop = 0
    end
    object CloseButton: TStyledButton
      AlignWithMargins = True
      Left = 410
      Top = 3
      Height = 30
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Close'
      TabOrder = 5
      StyleClass = 'Info'
      ExplicitLeft = 407
      ExplicitTop = 4
    end
    object HelpButton: TStyledButton
      AlignWithMargins = True
      Left = 491
      Top = 3
      Height = 30
      Align = alRight
      OnClick = ButtonClick
      Visible = False
      Caption = 'Help'
      TabOrder = 6
      StyleClass = 'Dark'
      ExplicitLeft = 441
      ExplicitTop = -1
    end
  end
  object SVGIconImageList: TSVGIconImageList
    Size = 128
    SVGIconItems = <
      item
        IconName = 'Warning'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-alert" width="2' +
          '4" height="24" viewBox="0 0 24 24"><path d="M13,14H11V10H13M13,1' +
          '8H11V16H13M1,21H23L12,2L1,21Z" /></svg>'
        FixedColor = 3130077
      end
      item
        IconName = 'Error'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-close-circle" w' +
          'idth="24" height="24" viewBox="0 0 24 24"><path d="M12,2C17.53,2' +
          ' 22,6.47 22,12C22,17.53 17.53,22 12,22C6.47,22 2,17.53 2,12C2,6.' +
          '47 6.47,2 12,2M15.59,7L12,10.59L8.41,7L7,8.41L10.59,12L7,15.59L8' +
          '.41,17L12,13.41L15.59,17L17,15.59L13.41,12L17,8.41L15.59,7Z" /><' +
          '/svg>'
        FixedColor = 1846997
      end
      item
        IconName = 'Information'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-information" wi' +
          'dth="24" height="24" viewBox="0 0 24 24"><path d="M13,9H11V7H13M' +
          '13,17H11V11H13M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0' +
          ',0 22,12A10,10 0 0,0 12,2Z" /></svg>'
        FixedColor = 14843950
      end
      item
        IconName = 'Confirmation'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-emoticon" width' +
          '="24" height="24" viewBox="0 0 24 24"><path d="M12,2C6.47,2 2,6.' +
          '5 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M15.5' +
          ',8A1.5,1.5 0 0,1 17,9.5A1.5,1.5 0 0,1 15.5,11A1.5,1.5 0 0,1 14,9' +
          '.5A1.5,1.5 0 0,1 15.5,8M8.5,8A1.5,1.5 0 0,1 10,9.5A1.5,1.5 0 0,1' +
          ' 8.5,11A1.5,1.5 0 0,1 7,9.5A1.5,1.5 0 0,1 8.5,8M12,17.5C9.67,17.' +
          '5 7.69,16.04 6.89,14H17.11C16.3,16.04 14.33,17.5 12,17.5Z" /></s' +
          'vg>'
        FixedColor = 2470209
      end
      item
        IconName = 'Custom'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-comment-questio' +
          'n" width="24" height="24" viewBox="0 0 24 24"><path d="M4,2H20A2' +
          ',2 0 0,1 22,4V16A2,2 0 0,1 20,18H13.9L10.2,21.71C10,21.9 9.75,22' +
          ' 9.5,22V22H9A1,1 0 0,1 8,21V18H4A2,2 0 0,1 2,16V4C2,2.89 2.9,2 4' +
          ',2M12.19,5.5C11.3,5.5 10.59,5.68 10.05,6.04C9.5,6.4 9.22,7 9.27,' +
          '7.69H11.24C11.24,7.41 11.34,7.2 11.5,7.06C11.7,6.92 11.92,6.85 1' +
          '2.19,6.85C12.5,6.85 12.77,6.93 12.95,7.11C13.13,7.28 13.22,7.5 1' +
          '3.22,7.8C13.22,8.08 13.14,8.33 13,8.54C12.83,8.76 12.62,8.94 12.' +
          '36,9.08C11.84,9.4 11.5,9.68 11.29,9.92C11.1,10.16 11,10.5 11,11H' +
          '13C13,10.72 13.05,10.5 13.14,10.32C13.23,10.15 13.4,10 13.66,9.8' +
          '5C14.12,9.64 14.5,9.36 14.79,9C15.08,8.63 15.23,8.24 15.23,7.8C1' +
          '5.23,7.1 14.96,6.54 14.42,6.12C13.88,5.71 13.13,5.5 12.19,5.5M11' +
          ',12V14H13V12H11Z" /></svg>'
        FixedColor = 5746666
      end>
    Scaled = True
    Left = 295
    Top = 136
  end
end
