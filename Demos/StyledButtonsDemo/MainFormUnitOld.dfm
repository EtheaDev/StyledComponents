object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Styled Buttons (c) Copyright Ethea S.r.l.'
  ClientHeight = 691
  ClientWidth = 874
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object StyledButton: TStyledButton
    Left = 6
    Top = 646
    Width = 129
    Height = 41
    OnClick = TestActionExecute
    Caption = 'StyledButton'
    ImageIndex = 1
    Images = SVGIconImageList
    TabOrder = 1
  end
  object NormalButton: TButton
    Left = 155
    Top = 646
    Width = 139
    Height = 41
    Caption = 'NormalButton'
    TabOrder = 0
    OnMouseDown = ButtonMouseDown
  end
  object ActionList: TActionList
    Left = 404
    Top = 442
    object TestAction: TAction
      Caption = 'Action'
      ImageIndex = 9
      OnExecute = TestActionExecute
    end
  end
  object SVGIconImageList: TSVGIconImageList
    Size = 32
    SVGIconItems = <
      item
        IconName = 'about'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#219' +
          '6F3" d="M37,40H11l-6,6V12c0-3.3,2.7-6,6-6h26c3.3,0,6,2.7,6,6v22C' +
          '43,37.3,40.3,40,37,40z"/>'#13#10'    <g fill="#fff">'#13#10'        <rect x=' +
          '"22" y="20" width="4" height="11"/>'#13#10'        <circle cx="24" cy=' +
          '"15" r="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'accept_database'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#D1C4E9' +
          '">'#13#10'        <path d="M38,7H10C8.9,7,8,7.9,8,9v6c0,1.1,0.9,2,2,2h' +
          '28c1.1,0,2-0.9,2-2V9C40,7.9,39.1,7,38,7z"/>'#13#10'        <path d="M3' +
          '8,19H10c-1.1,0-2,0.9-2,2v6c0,1.1,0.9,2,2,2h28c1.1,0,2-0.9,2-2v-6' +
          'C40,19.9,39.1,19,38,19z"/>'#13#10'        <path d="M38,31H10c-1.1,0-2,' +
          '0.9-2,2v6c0,1.1,0.9,2,2,2h28c1.1,0,2-0.9,2-2v-6C40,31.9,39.1,31,' +
          '38,31z"/>'#13#10'    </g>'#13#10'    <circle fill="#43A047" cx="38" cy="38" ' +
          'r="10"/>'#13#10'    <polygon fill="#DCEDC8" points="42.5,33.3 36.8,39 ' +
          '34.1,36.3 32,38.5 36.8,43.3 44.6,35.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'add_column'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90C' +
          'AF9" d="M30,5H18c-2.2,0-4,1.8-4,4v30c0,2.2,1.8,4,4,4h12c2.2,0,4-' +
          '1.8,4-4V9C34,6.8,32.2,5,30,5z M18,39V9h12l0,30 H18z"/>'#13#10'    <cir' +
          'cle fill="#43A047" cx="38" cy="38" r="10"/>'#13#10'    <g fill="#fff">' +
          #13#10'        <rect x="36" y="32" width="4" height="12"/>'#13#10'        <' +
          'rect x="32" y="36" width="12" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'add_database'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#D1C4E9' +
          '">'#13#10'        <path d="M38,7H10C8.9,7,8,7.9,8,9v6c0,1.1,0.9,2,2,2h' +
          '28c1.1,0,2-0.9,2-2V9C40,7.9,39.1,7,38,7z"/>'#13#10'        <path d="M3' +
          '8,19H10c-1.1,0-2,0.9-2,2v6c0,1.1,0.9,2,2,2h28c1.1,0,2-0.9,2-2v-6' +
          'C40,19.9,39.1,19,38,19z"/>'#13#10'        <path d="M38,31H10c-1.1,0-2,' +
          '0.9-2,2v6c0,1.1,0.9,2,2,2h28c1.1,0,2-0.9,2-2v-6C40,31.9,39.1,31,' +
          '38,31z"/>'#13#10'    </g>'#13#10'    <circle fill="#43A047" cx="38" cy="38" ' +
          'r="10"/>'#13#10'    <g fill="#fff">'#13#10'        <rect x="36" y="32" width' +
          '="4" height="12"/>'#13#10'        <rect x="32" y="36" width="12" heigh' +
          't="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'add_image'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8CB' +
          'CD6" d="M40,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v26C44,39.2,42.2,41,40,41z"/>'#13#10'    <circle fill="#B3DDF5"' +
          ' cx="35" cy="16" r="3"/>'#13#10'    <polygon fill="#9AC9E3" points="20' +
          ',16 9,32 31,32"/>'#13#10'    <polygon fill="#B3DDF5" points="31,22 23,' +
          '32 39,32"/>'#13#10'    <circle fill="#43A047" cx="38" cy="38" r="10"/>' +
          #13#10'    <g fill="#fff">'#13#10'        <rect x="36" y="32" width="4" hei' +
          'ght="12"/>'#13#10'        <rect x="32" y="36" width="12" height="4"/>'#13 +
          #10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'add_row'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90C' +
          'AF9" d="M43,30V18c0-2.2-1.8-4-4-4H9c-2.2,0-4,1.8-4,4v12c0,2.2,1.' +
          '8,4,4,4h30C41.2,34,43,32.2,43,30z M9,18h30v12 L9,30V18z"/>'#13#10'    ' +
          '<circle fill="#43A047" cx="38" cy="38" r="10"/>'#13#10'    <g fill="#f' +
          'ff">'#13#10'        <rect x="32" y="36" width="12" height="4"/>'#13#10'     ' +
          '   <rect x="36" y="32" width="4" height="12"/>'#13#10'    </g>'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'address_book'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#673' +
          'AB7" d="M38,44H12V4h26c2.2,0,4,1.8,4,4v32C42,42.2,40.2,44,38,44z' +
          '"/>'#13#10'    <path fill="#311B92" d="M10,4h2v40h-2c-2.2,0-4-1.8-4-4V' +
          '8C6,5.8,7.8,4,10,4z"/>'#13#10'    <path fill="#fff" d="M36,24.2c-0.1,4' +
          '.8-3.1,6.9-5.3,6.7c-0.6-0.1-2.1-0.1-2.9-1.6c-0.8,1-1.8,1.6-3.1,1' +
          '.6c-2.6,0-3.3-2.5-3.4-3.1 c-0.1-0.7-0.2-1.4-0.1-2.2c0.1-1,1.1-6.' +
          '5,5.7-6.5c2.2,0,3.5,1.1,3.7,1.3L30,27.2c0,0.3-0.2,1.6,1.1,1.6c2.' +
          '1,0,2.4-3.9,2.4-4.6 c0.1-1.2,0.3-8.2-7-8.2c-6.9,0-7.9,7.4-8,9.2c' +
          '-0.5,8.5,6,8.5,7.2,8.5c1.7,0,3.7-0.7,3.9-0.8l0.4,2c-0.3,0.2-2,1.' +
          '1-4.4,1.1 c-2.2,0-10.1-0.4-9.8-10.8C16.1,23.1,17.4,14,26.6,14C35' +
          '.8,14,36,22.1,36,24.2z M24.1,25.5c-0.1,1,0,1.8,0.2,2.3 c0.2,0.5,' +
          '0.6,0.8,1.2,0.8c0.1,0,0.3,0,0.4-0.1c0.2-0.1,0.3-0.1,0.5-0.3c0.2-' +
          '0.1,0.3-0.3,0.5-0.6c0.2-0.2,0.3-0.6,0.4-1l0.5-5.4 c-0.2-0.1-0.5-' +
          '0.1-0.7-0.1c-0.5,0-0.9,0.1-1.2,0.3c-0.3,0.2-0.6,0.5-0.9,0.8c-0.2' +
          ',0.4-0.4,0.8-0.6,1.3S24.2,24.8,24.1,25.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'advance'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#1565C0' +
          '">'#13#10'        <polygon points="46.1,24 33,35 33,13"/>'#13#10'        <re' +
          'ct x="10" y="20" width="4" height="8"/>'#13#10'        <rect x="4" y="' +
          '20" width="4" height="8"/>'#13#10'        <rect x="16" y="20" width="4' +
          '" height="8"/>'#13#10'        <rect x="22" y="20" width="14" height="8' +
          '"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'advertising'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#90CAF9' +
          '">'#13#10'        <path d="M17.4,33H15v-4h4l0.4,1.5C19.7,31.8,18.7,33,' +
          '17.4,33z"/>'#13#10'        <path d="M37,36c0,0-11.8-7-18-7V15c5.8,0,18' +
          '-7,18-7V36z"/>'#13#10'    </g>'#13#10'    <g fill="#283593">'#13#10'        <circl' +
          'e cx="9" cy="22" r="5"/>'#13#10'        <path d="M40,19h-3v6h3c1.7,0,3' +
          '-1.3,3-3S41.7,19,40,19z"/>'#13#10'        <path d="M18.6,41.2c-0.9,0.6' +
          '-2.5,1.2-4.6,1.4c-0.6,0.1-1.2-0.3-1.4-1L8.2,27.9c0,0,8.8-6.2,8.8' +
          ',1.1 c0,5.5,1.5,8.4,2.2,9.5c0.5,0.7,0.5,1.6,0,2.3C19,41,18.8,41.' +
          '1,18.6,41.2z"/>'#13#10'    </g>'#13#10'    <path fill="#3F51B5" d="M9,29h10V' +
          '15H9c-1.1,0-2,0.9-2,2v10C7,28.1,7.9,29,9,29z"/>'#13#10'    <path fill=' +
          '"#42A5F5" d="M38,38L38,38c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-2h0c1' +
          '.1,0,2,0.9,2,2v28C40,37.1,39.1,38,38,38z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'alarm_clock'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#37474F' +
          '">'#13#10'        <path d="M38.5,44.6l-4-4l2.1-2.1l4,4c0.6,0.6,0.6,1.5' +
          ',0,2.1l0,0C40.1,45.1,39.1,45.1,38.5,44.6z"/>'#13#10'        <path d="M' +
          '9.5,44.6l4-4l-2.1-2.1l-4,4c-0.6,0.6-0.6,1.5,0,2.1l0,0C7.9,45.1,8' +
          '.9,45.1,9.5,44.6z"/>'#13#10'    </g>'#13#10'    <circle fill="#C62828" cx="2' +
          '4" cy="24" r="20"/>'#13#10'    <circle fill="#eee" cx="24" cy="24" r="' +
          '16"/>'#13#10'    <rect x="19" y="22.1" transform="matrix(-.707 -.707 .' +
          '707 -.707 12.904 62.537)" fill="#E53935" width=".8" height="13"/' +
          '>'#13#10'    <rect x="23" y="11" width="2" height="13"/>'#13#10'    <rect x=' +
          '"26.1" y="22.7" transform="matrix(-.707 .707 -.707 -.707 65.787 ' +
          '27.25)" width="2.3" height="9.2"/>'#13#10'    <circle cx="24" cy="24" ' +
          'r="2"/>'#13#10'    <circle fill="#C62828" cx="24" cy="24" r="1"/>'#13#10'   ' +
          ' <rect x="22" y="1" fill="#37474F" width="4" height="3"/>'#13#10'    <' +
          'g fill="#37474F">'#13#10'        <path d="M44.4,16.2c2.5-3.5,2.1-8.4-1' +
          '-11.5c-3.1-3.1-8-3.5-11.5-1L44.4,16.2z"/>'#13#10'        <path d="M3.6' +
          ',16.2c-2.5-3.5-2.1-8.4,1-11.5c3.1-3.1,8-3.5,11.5-1L3.6,16.2z"/>'#13 +
          #10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'alphabetical_sorting_az'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '546E7A" points="38,33 38,5 34,5 34,33 28,33 36,43 44,33"/>'#13#10'    ' +
          '<g fill="#2196F3">'#13#10'        <path d="M16.8,17.2h-5.3l-1.1,3H6.9L' +
          '12.6,5h2.9l5.7,15.2h-3.2L16.8,17.2z M12.2,14.5H16l-1.9-5.7L12.2,' +
          '14.5z"/>'#13#10'        <path d="M12.4,40.5H20V43H8.4v-1.9L16,30.3H8.4' +
          'v-2.5h11.4v1.7L12.4,40.5z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'alphabetical_sorting_za'
        SVGText = 
          #65279'<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 ' +
          '0 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#2196F' +
          '3">'#13#10'        <path d="M16.8,40h-5.3l-1.1,3H6.9l5.7-15.2h2.9L21.1' +
          ',43h-3.2L16.8,40z M12.2,37.3H16l-1.9-5.7L12.2,37.3z"/>'#13#10'        ' +
          '<path d="M12.4,17.7H20v2.5H8.4v-1.9L16,7.5H8.4V5h11.4v1.7L12.4,1' +
          '7.7z"/>'#13#10'    </g>'#13#10'    <polygon fill="#546E7A" points="38,33 38,' +
          '5 34,5 34,33 28,33 36,43 44,33"/>'#13#10'</svg>'
      end
      item
        IconName = 'android_os'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<g>'#13#10#9'<path fill="#7CB342" d="M12,29.001c0,1.104-0.896,' +
          '2-2,2l0,0c-1.104,0-2-0.896-2-2v-9c0-1.104,0.896-2,2-2l0,0c1.104,' +
          '0,2,0.896,2,2'#13#10#9#9'V29.001z"/>'#13#10#9'<path fill="#7CB342" d="M40,29.00' +
          '1c0,1.104-0.896,2-2,2l0,0c-1.104,0-2-0.896-2-2v-9c0-1.104,0.896-' +
          '2,2-2l0,0c1.104,0,2,0.896,2,2'#13#10#9#9'V29.001z"/>'#13#10#9'<path fill="#7CB3' +
          '42" d="M22,40c0,1.104-0.896,2-2,2l0,0c-1.104,0-2-0.896-2-2v-9c0-' +
          '1.104,0.896-2,2-2l0,0c1.104,0,2,0.896,2,2V40z"'#13#10#9#9'/>'#13#10#9'<path fil' +
          'l="#7CB342" d="M30,40c0,1.104-0.896,2-2,2l0,0c-1.104,0-2-0.896-2' +
          '-2v-9c0-1.104,0.896-2,2-2l0,0c1.104,0,2,0.896,2,2V40z"'#13#10#9#9'/>'#13#10#9'<' +
          'path fill="#7CB342" d="M14,18.001V33c0,1.104,0.896,2,2,2h16c1.10' +
          '4,0,2-0.896,2-2V18.001H14z"/>'#13#10#9'<path fill="#7CB342" d="M24,8c-6' +
          ',0-9.655,3.645-10,8h20C33.654,11.645,30,8,24,8z M20,13.598c-0.55' +
          '2,0-1-0.448-1-1s0.448-1,1-1'#13#10#9#9's1,0.448,1,1S20.552,13.598,20,13.' +
          '598z M28,13.598c-0.553,0-1-0.448-1-1s0.447-1,1-1s1,0.448,1,1S28.' +
          '553,13.598,28,13.598z"/>'#13#10#9'<line fill="none" stroke="#7CB342" st' +
          'roke-width="2" stroke-linecap="round" x1="30" y1="7" x2="28.334"' +
          ' y2="9.499"/>'#13#10#9'<line fill="none" stroke="#7CB342" stroke-width=' +
          '"2" stroke-linecap="round" x1="18" y1="7" x2="19.333" y2="9.082"' +
          '/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'answers'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '42A5F5" points="36,44 8,44 8,8 28,8 36,16"/>'#13#10'    <polygon fill=' +
          '"#90CAF9" points="40,40 12,40 12,4 32,4 40,12"/>'#13#10'    <polygon f' +
          'ill="#E1F5FE" points="38.5,13 31,13 31,5.5"/>'#13#10'    <path fill="#' +
          '1976D2" d="M23.4,29.9c0-0.2,0-0.4,0.1-0.6s0.2-0.3,0.3-0.5s0.3-0.' +
          '2,0.5-0.3s0.4-0.1,0.6-0.1s0.5,0,0.7,0.1 s0.4,0.2,0.5,0.3s0.2,0.3' +
          ',0.3,0.5s0.1,0.4,0.1,0.6s0,0.4-0.1,0.6s-0.2,0.3-0.3,0.5s-0.3,0.2' +
          '-0.5,0.3s-0.4,0.1-0.7,0.1 s-0.5,0-0.6-0.1s-0.4-0.2-0.5-0.3s-0.2-' +
          '0.3-0.3-0.5S23.4,30.1,23.4,29.9z M26.1,26.8h-2.3L23.4,17h3L26.1,' +
          '26.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'approval'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '8BC34A" points="24,3 28.7,6.6 34.5,5.8 36.7,11.3 42.2,13.5 41.4,' +
          '19.3 45,24 41.4,28.7 42.2,34.5 36.7,36.7 34.5,42.2 28.7,41.4 24,' +
          '45 19.3,41.4 13.5,42.2 11.3,36.7 5.8,34.5 6.6,28.7 3,24 6.6,19.3' +
          ' 5.8,13.5 11.3,11.3 13.5,5.8 19.3,6.6"/>'#13#10'    <polygon fill="#CC' +
          'FF90" points="34.6,14.6 21,28.2 15.4,22.6 12.6,25.4 21,33.8 37.4' +
          ',17.4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'approve'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFCC80' +
          '">'#13#10'        <circle cx="38" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"10" cy="26" r="4"/>'#13#10'        <path d="M39,19c0-12.7-30-8.3-30,0' +
          'c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7,15-15C39,27.2,39,20.' +
          '8,39,19z"/>'#13#10'        <path d="M24,4C15.2,4,8,11.2,8,20c0,1.2,0,3' +
          '.5,0,3.5l2.1,0.6V19l19.5-6.3l8.2,6.3v5.1l2.1-0.6c0,0,0-2.3,0-3.5' +
          ' C40,12.5,34.6,4,24,4z"/>'#13#10'    </g>'#13#10'    <polygon fill="#4CAF50"' +
          ' points="32.6,18.6 22.3,28.9 17.4,24 14.6,26.8 22.3,34.5 35.4,21' +
          '.4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'area_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="42,37 6,37 6,25 16,10 30,17 42,6"/>'#13#10'    <polygo' +
          'n fill="#00BCD4" points="42,42 6,42 6,32 16,24 30,26 42,17"/>'#13#10'<' +
          '/svg>'#13#10
      end
      item
        IconName = 'assistant'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFA726' +
          '">'#13#10'        <circle cx="10" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"38" cy="26" r="4"/>'#13#10'    </g>'#13#10'    <path fill="#FFB74D" d="M39,' +
          '19c0-12.7-30-8.3-30,0c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7' +
          ',15-15C39,27.2,39,20.8,39,19z"/>'#13#10'    <path fill="#FF5722" d="M2' +
          '4,3C14.6,3,7,10.6,7,20c0,1.2,0,3.4,0,3.4L9,25v-3l21-9.8l9,9.8v3l' +
          '2-1.6c0,0,0-2.1,0-3.4 C41,12,35.3,3,24,3z"/>'#13#10'    <g fill="#7847' +
          '19">'#13#10'        <circle cx="31" cy="26" r="2"/>'#13#10'        <circle c' +
          'x="17" cy="26" r="2"/>'#13#10'    </g>'#13#10'    <path fill="#757575" d="M4' +
          '3,24c-0.6,0-1,0.4-1,1v-7c0-8.8-7.2-16-16-16h-7c-0.6,0-1,0.4-1,1s' +
          '0.4,1,1,1h7c7.7,0,14,6.3,14,14v10 c0,0.6,0.4,1,1,1s1-0.4,1-1v2c0' +
          ',3.9-3.1,7-7,7H24c-0.6,0-1,0.4-1,1s0.4,1,1,1h11c5,0,9-4,9-9v-5C4' +
          '4,24.4,43.6,24,43,24z"/>'#13#10'    <g fill="#37474F">'#13#10'        <path ' +
          'd="M43,22h-1c-1.1,0-2,0.9-2,2v4c0,1.1,0.9,2,2,2h1c1.1,0,2-0.9,2-' +
          '2v-4C45,22.9,44.1,22,43,22z"/>'#13#10'        <circle cx="24" cy="38" ' +
          'r="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end>
    Left = 432
    Top = 352
  end
end
