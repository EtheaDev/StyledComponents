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
  OnClick = FormClick
  TextHeight = 13
  object StyledButton: TStyledButton
    Left = 8
    Top = 642
    Width = 129
    Height = 41
    OnClick = TestActionExecute
    Caption = 'Action'
    ImageIndex = 1
    ImageName = 'accept_database'
    Images = VirtualImageList
    TabOrder = 1
  end
  object NormalButton: TButton
    Left = 155
    Top = 642
    Width = 139
    Height = 41
    Action = TestAction
    ImageIndex = 1
    ImageName = 'accept_database'
    Images = VirtualImageList
    TabOrder = 0
    OnMouseDown = ButtonMouseDown
  end
  object VirtualImageList: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'about'
        Name = 'about'
      end
      item
        CollectionIndex = 1
        CollectionName = 'accept_database'
        Name = 'accept_database'
      end
      item
        CollectionIndex = 2
        CollectionName = 'add_column'
        Name = 'add_column'
      end
      item
        CollectionIndex = 3
        CollectionName = 'add_database'
        Name = 'add_database'
      end
      item
        CollectionIndex = 4
        CollectionName = 'add_image'
        Name = 'add_image'
      end
      item
        CollectionIndex = 5
        CollectionName = 'add_row'
        Name = 'add_row'
      end
      item
        CollectionIndex = 6
        CollectionName = 'address_book'
        Name = 'address_book'
      end
      item
        CollectionIndex = 7
        CollectionName = 'advance'
        Name = 'advance'
      end
      item
        CollectionIndex = 8
        CollectionName = 'advertising'
        Name = 'advertising'
      end
      item
        CollectionIndex = 9
        CollectionName = 'alarm_clock'
        Name = 'alarm_clock'
      end
      item
        CollectionIndex = 10
        CollectionName = 'alphabetical_sorting_az'
        Name = 'alphabetical_sorting_az'
      end
      item
        CollectionIndex = 11
        CollectionName = 'alphabetical_sorting_za'
        Name = 'alphabetical_sorting_za'
      end>
    ImageCollection = SVGIconImageCollection
    Width = 32
    Height = 32
    Left = 278
    Top = 209
  end
  object SVGIconImageCollection: TSVGIconImageCollection
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
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#2196F3' +
          '">'#13#10'        <path d="M16.8,40h-5.3l-1.1,3H6.9l5.7-15.2h2.9L21.1,' +
          '43h-3.2L16.8,40z M12.2,37.3H16l-1.9-5.7L12.2,37.3z"/>'#13#10'        <' +
          'path d="M12.4,17.7H20v2.5H8.4v-1.9L16,7.5H8.4V5h11.4v1.7L12.4,17' +
          '.7z"/>'#13#10'    </g>'#13#10'    <polygon fill="#546E7A" points="38,33 38,5' +
          ' 34,5 34,33 28,33 36,43 44,33"/>'#13#10'</svg>'
      end>
    Left = 294
    Top = 277
  end
  object ActionList: TActionList
    Images = VirtualImageList
    Left = 404
    Top = 442
    object TestAction: TAction
      Caption = 'Action'
      ImageIndex = 9
      ImageName = 'alarm_clock'
      OnExecute = TestActionExecute
    end
  end
end
