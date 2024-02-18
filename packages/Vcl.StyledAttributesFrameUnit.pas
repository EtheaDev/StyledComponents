unit Vcl.StyledAttributesFrameUnit;

interface

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.ComCtrls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.StyledButton
  , Vcl.Samples.Spin
  , Vcl.ButtonStylesAttributes
  ;

type
  TAttributesFrame = class(TFrame)
    TopPanel: TPanel;
    StyledButton: TStyledButton;
    ButtonDrawingStyleGroupBox: TGroupBox;
    BorderGroupBox: TGroupBox;
    BorderColorPanel: TPanel;
    BorderColorLabel: TLabel;
    BorderColorBox: TColorBox;
    BorderDrawStyleRadioGroup: TRadioGroup;
    BorderWidthPanel: TPanel;
    BorderDrawWidthLabel: TLabel;
    BorderWidthSpinEdit: TSpinEdit;
    FontGroupBox: TGroupBox;
    FontColorPanel: TPanel;
    FontColorLabel: TLabel;
    FontColorBox: TColorBox;
    FontStyleGroupBox: TGroupBox;
    FontBoldCheckBox: TCheckBox;
    UnderlineCheckBox: TCheckBox;
    ItalicCheckBox: TCheckBox;
    StrikeoutCheckBox: TCheckBox;
    ButtonFaceGroupBox: TGroupBox;
    ButtonColorPanel: TPanel;
    ButtonDrawStyleRadioGroup: TRadioGroup;
    ButtonColorBox: TColorBox;
    ButtonColorLabel: TLabel;
    ButtonDrawTypePanel: TPanel;
    StyleDrawTypeLabel: TLabel;
    StyleDrawTypeComboBox: TComboBox;
    RadiusPanel: TPanel;
    StyleRadiusLabel: TLabel;
    RadiusTrackBar: TTrackBar;
    ButtonStyleLabel: TLabel;
  private
    FStyledButtonAttributes: TStyledButtonAttributes;
    procedure UpdateGUIFromAttributes;
    procedure UpdateAttributesFromGUI;
    procedure SetStyledButtonAttributes(const Value: TStyledButtonAttributes);
    { Private declarations }
  public
    procedure Setup;
    property StyledButtonAttributes: TStyledButtonAttributes read FStyledButtonAttributes write SetStyledButtonAttributes;
  end;

implementation

{$R *.dfm}

uses
  System.TypInfo;

{ TAttributesFrame }

procedure TAttributesFrame.SetStyledButtonAttributes(
  const Value: TStyledButtonAttributes);
begin
  if FStyledButtonAttributes <> Value then
    UpdateGUIFromAttributes;
end;

procedure TAttributesFrame.Setup;
var
  I: TStyledButtonDrawType;
  LPos: Integer;
  LDrawName: string;
begin
  for I := Low(TStyledButtonDrawType) to High(TStyledButtonDrawType) do
  begin
    LDrawName := GetEnumName(TypeInfo(TStyledButtonDrawType), Ord(I));
    LPos := StyleDrawTypeComboBox.Items.Add(LDrawName);
    if I = FStyledButtonAttributes.DrawType then
      StyleDrawTypeComboBox.ItemIndex := LPos;
  end;
  //Font size and bold for Label
  ButtonStyleLabel.Font.Height := ButtonStyleLabel.Font.Height * 2;
  ButtonStyleLabel.Font.Style := [fsBold];
end;

procedure TAttributesFrame.UpdateAttributesFromGUI;
var
  LFontStyle: TFontStyles;
begin
  FStyledButtonAttributes.BorderColor := BorderColorBox.Selected;
  FStyledButtonAttributes.BorderDrawStyle := TBorderDrawStyle(BorderDrawStyleRadioGroup.ItemIndex);
  FStyledButtonAttributes.BorderWidth := BorderWidthSpinEdit.Value;
  FStyledButtonAttributes.FontColor := FontColorBox.Selected;
  LFontStyle := [];
  if FontBoldCheckBox.Checked then
    LFontStyle := LFontStyle + [fsBold];
  if UnderlineCheckBox.Checked then
    LFontStyle := LFontStyle + [fsUnderline];
  if ItalicCheckBox.Checked then
    LFontStyle := LFontStyle + [fsItalic];
  if StrikeoutCheckBox.Checked then
    LFontStyle := LFontStyle + [fsStrikeOut];
  FStyledButtonAttributes.FontStyle := LFontStyle;
  FStyledButtonAttributes.ButtonDrawStyle := TButtonDrawStyle(ButtonDrawStyleRadioGroup.ItemIndex);
  FStyledButtonAttributes.ButtonColor := ButtonColorBox.Selected;
  FStyledButtonAttributes.DrawType := TStyledButtonDrawType(StyleDrawTypeComboBox.ItemIndex);
  FStyledButtonAttributes.Radius := RadiusTrackBar.Position;
end;

procedure TAttributesFrame.UpdateGUIFromAttributes;
begin
  BorderColorBox.Selected := FStyledButtonAttributes.BorderColor;
  BorderDrawStyleRadioGroup.ItemIndex := Ord(FStyledButtonAttributes.BorderDrawStyle);
  BorderWidthSpinEdit.Value := FStyledButtonAttributes.BorderWidth;
  FontColorBox.Selected := FStyledButtonAttributes.FontColor;
  FontBoldCheckBox.Checked := fsBold in FStyledButtonAttributes.FontStyle;
  UnderlineCheckBox.Checked := fsUnderline in FStyledButtonAttributes.FontStyle;
  ItalicCheckBox.Checked := fsItalic in FStyledButtonAttributes.FontStyle;
  StrikeoutCheckBox.Checked := fsStrikeOut in FStyledButtonAttributes.FontStyle;
  ButtonDrawStyleRadioGroup.ItemIndex := Ord(FStyledButtonAttributes.ButtonDrawStyle);
  ButtonColorBox.Selected := FStyledButtonAttributes.ButtonColor;
  StyleDrawTypeComboBox.ItemIndex :=  Ord(FStyledButtonAttributes.DrawType);
  RadiusTrackBar.Position := FStyledButtonAttributes.Radius;
end;

end.
