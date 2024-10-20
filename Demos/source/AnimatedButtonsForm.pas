unit AnimatedButtonsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StyledButton, Vcl.StdCtrls,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList, Vcl.StdActns,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StyledAnimatedButton,
  Vcl.ButtonStylesAttributes;

type
  TButtonProc = reference to procedure (Button: TStyledAnimatedButton);

  TfmAnimatedButtons = class(TForm)
    ActionList: TActionList;
    acFileOpen: TFileOpen;
    Images: TImageList;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    ButtonsPanel: TPanel;
    ImageAlignmentRadioGroup: TRadioGroup;
    AutoAnimationRadioGroup: TRadioGroup;
    cbAnimateOnClick: TCheckBox;
    cbAnimateOnFocused: TCheckBox;
    cbAnimateAlways: TCheckBox;
    cbAnimateOnMouseOver: TCheckBox;
    StyledAnimatedButton: TStyledAnimatedButton;
    procedure ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VCLButtonKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure acFileOpenBeforeExecute(Sender: TObject);
    procedure ImageAlignmentRadioGroupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AutoAnimationClick(Sender: TObject);
  private
    FStyleNames: TStringList;
    function CreateNewButton(const AParent: TWinControl;
      const ALeft, ATop: Integer;
      const AImageName, AStyle: string): TStyledAnimatedButton;
    function GetAnimTypes: TAutoAnimationTypes;
    procedure ProcessButtons(AButtonProc: TButtonProc);
  public
  end;

var
  fmAnimatedButtons: TfmAnimatedButtons;

implementation

{$R *.dfm}

uses
  Vcl.StyledTaskDialog;

procedure TfmAnimatedButtons.acFileOpenBeforeExecute(Sender: TObject);
begin
  ;
end;

procedure TfmAnimatedButtons.VCLButtonKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StyledShowMessage('Esc');
end;

procedure TfmAnimatedButtons.ButtonClick(Sender: TObject);
var
  LCaption: TCaption;
  LMsgDlgType: TMsgDlgType;
begin
  if Sender is TButton then
    LCaption := TButton(Sender).Caption
  else if Sender is TStyledButton then
    LCaption := TStyledButton(Sender).Caption
  else if Sender is TStyledGraphicButton then
    LCaption := TStyledGraphicButton(Sender).Caption;

  LCaption := StringReplace(LCaption,'&','', [rfReplaceAll]);

  if LCaption = 'Information' then
    LMsgDlgType := TMsgDlgType.mtInformation
  else if LCaption = 'Warning' then
    LMsgDlgType := TMsgDlgType.mtWarning
  else if LCaption = 'Error' then
    LMsgDlgType := TMsgDlgType.mtError
  else if LCaption = 'Question' then
    LMsgDlgType := TMsgDlgType.mtConfirmation
  else if LCaption = 'Notify' then
    LMsgDlgType := TMsgDlgType.mtInformation
  else if LCaption = 'Custom' then
    LMsgDlgType := TMsgDlgType.mtCustom
  else
    LMsgDlgType := TMsgDlgType.mtInformation;

  StyledMessageDlg(Format('"%s" clicked!%sSearch more projects at: <A HREF="https://github.com/EtheaDev">github.com/EtheaDev</A>',
    [LCaption, sLineBreak]),
    LMsgDlgType, [mbYes, mbNo, mbOK, mbCancel], 0);
end;

procedure TfmAnimatedButtons.ProcessButtons(
  AButtonProc: TButtonProc);
var
  LButton: TStyledAnimatedButton;
  I: Integer;
begin
  for I := 0 to ComponentCount -1 do
  begin
    if Components[I] is TStyledAnimatedButton then
    begin
      LButton := TStyledAnimatedButton(Components[I]);
      AButtonProc(LButton);
    end;
  end;
end;

function TfmAnimatedButtons.CreateNewButton(const AParent: TWinControl; const ALeft,
  ATop: Integer; const AImageName, AStyle: string): TStyledAnimatedButton;
var
  LIconName, LResName: string;
  LFileName: TFileName;
begin
  Result := TStyledAnimatedButton.CreateStyled(
    Self, 'Bootstrap', AImageName, 'Normal');
  Result.StyleRadius := 20;
  LIconName := StringReplace(AImageName,'&','', [rfReplaceAll]);
  Result.Name := LIconName;
  Result.AutoSizeAnimationMargin := 20;
  Result.SetBounds(ALeft,ATop,220,80);
  Result.Parent := AParent;
  //Result.AnimationWidth := 42;
  //Result.AnimationHeight := 42;
  //Result.Images := VirtualImageList;
  //Result.ImageName := AImageName;
  //Result.ImageAlignment := TImageAlignment.iaTop;
  LFileName := Format(ExtractFilePath(Application.ExeName)+'..\..\..\..\Animations\%s.json', [LIconName]);
  LResName := UpperCase(Format('LOTTIE_%s',[LIconName]));
  Result.LoadAnimationFromResource(LResName);
  //Result.LoadAnimationFromFile(LFileName);
  Result.Caption := AImageName;
  Result.StyleClass := AStyle;
  Result.AutoAnimationTypes := GetAnimTypes;
  Result.OnClick := ButtonClick;
end;

function TfmAnimatedButtons.GetAnimTypes: TAutoAnimationTypes;
begin
  Result := [];
  if cbAnimateOnMouseOver.Checked then
    Result := Result + [AnimateOnMouseOver];
  if cbAnimateOnClick.Checked then
    Result := Result + [AnimateOnClick];
  if cbAnimateAlways.Checked then
    Result := Result + [AnimateAlways];
  if cbAnimateOnFocused.Checked then
    Result := Result + [AnimateOnFocus];
end;

procedure TfmAnimatedButtons.AutoAnimationClick(Sender: TObject);
var
  LAutoAnimationTypes: TAutoAnimationTypes;
begin
  LAutoAnimationTypes := GetAnimTypes;
  ProcessButtons(
    procedure (AButton: TStyledAnimatedButton)
    begin
      AButton.AutoAnimationTypes := LAutoAnimationTypes;
    end);
end;

procedure TfmAnimatedButtons.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  CreateNewButton(Self, 10,  10,'&Information', 'Primary');
  CreateNewButton(Self, 10, 110,'&Warning', 'Danger');
  CreateNewButton(Self, 250, 10,'&Error', 'Warning');
  CreateNewButton(Self, 250,110,'&Question', 'Info');
  CreateNewButton(Self, 490, 10,'&Notify', 'Secondary');
  CreateNewButton(Self, 490,110,'&Custom', 'Dark');
end;

procedure TfmAnimatedButtons.FormDestroy(Sender: TObject);
begin
  FStyleNames.Free;
end;

procedure TfmAnimatedButtons.ImageAlignmentRadioGroupClick(Sender: TObject);
var
  LAlignment: TImageAlignment;
begin
  LAlignment := TImageAlignment(ImageAlignmentRadioGroup.ItemIndex-1);
  ProcessButtons(
    procedure (AButton: TStyledAnimatedButton)
    begin
      if ImageAlignmentRadioGroup.ItemIndex = 0 then
      begin
        AButton.Images := nil;
      end
      else
      begin
        AButton.Images := Images;
        AButton.ImageAlignment := LAlignment;
      end;
    end);
end;

end.
