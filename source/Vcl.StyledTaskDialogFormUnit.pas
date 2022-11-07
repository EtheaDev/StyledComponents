{******************************************************************************}
{                                                                              }
{       StyledTaskDialogForm: a Task Dialog Form with StyleButtons             }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Vcl.StyledTaskDialogFormUnit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  System.ImageList, //If you are using an older Delphi version, remove this line
  Vcl.StyledButton,
  Vcl.StyledTaskDialog,
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles;

type
  TTaskDialogLauncherHandler = class(TInterfacedObject, ITaskDialogLauncher)
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TCustomTaskDialog): Boolean;
  end;

  TStyledTaskDialogForm = class(TForm)
    BottomPanel: TPanel;
    CenterPanel: TPanel;
    ImagePanel: TPanel;
    ButtonsPanel: TPanel;
    OKButton: TStyledButton;
    AllButton: TStyledButton;
    BottomBevel: TBevel;
    MessageScrollBox: TScrollBox;
    TitleLabel: TLabel;
    TextLabel: TLinkLabel;
    AutoSizeLabel: TLabel;
    ImageList: TImageList;
    Image: TImage;
    CancelButton: TStyledButton;
    YesButton: TStyledButton;
    NoButton: TStyledButton;
    RetryButton: TStyledButton;
    CloseButton: TStyledButton;
    HelpButton: TStyledButton;
    AbortButton: TStyledButton;
    IgnoreButton: TStyledButton;
    NoToAllButton: TStyledButton;
    YesToAllButton: TStyledButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TextLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    FCustomIcons: TStyledDialogIcons;
    FTaskDialog: TCustomTaskDialog;
    FDialogType: TMsgDlgType;
    FCommonButtons: TTaskDialogCommonButtons;
    FDefaultButton: TTaskDialogCommonButton;
    FButtons: TTaskDialogButtons;
    procedure GetIconNameAndIndex(ATaskDialog: TMsgDlgType;
      out AImageName: string; out AImageIndex: Integer);
    procedure ShowDialogForm;
    procedure SetCommonButtons(const AValue: TTaskDialogCommonButtons);
    procedure SetHelpContext(const AValue: Integer);
    function GetHelpContext: Integer;
    function GetText: string;
    function GetTitle: string;
    procedure SetText(const AValue: string);
    procedure SetTitle(const AValue: string);
    procedure AdjustHeight;
    procedure AdjustWidth;
    procedure AdjustButtonsCaption;
    procedure SetButtons(const Value: TTaskDialogButtons);
    procedure PlayMessageDlgSound;
    procedure FocusDefaultButton;
    procedure LoadImage;
(*
    property Button: TTaskDialogButtonItem read FButton write FButton;
*)
    property Buttons: TTaskDialogButtons read FButtons write SetButtons;
    property CommonButtons: TTaskDialogCommonButtons read FCommonButtons write SetCommonButtons default [tcbOk, tcbCancel];
    property DefaultButton: TTaskDialogCommonButton read FDefaultButton write FDefaultButton default tcbOk;
(*
    property CustomFooterIcon: TIcon read FCustomFooterIcon write SetCustomFooterIcon;
    property CustomMainIcon: TIcon read FCustomMainIcon write SetCustomMainIcon;
    property ExpandButtonCaption: string read FExpandButtonCaption write FExpandButtonCaption;
    property Expanded: Boolean read FExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property FooterIcon: TTaskDialogIcon read FFooterIcon write SetFooterIcon default tdiNone;
    property FooterText: string read FFooterText write SetFooterText;
    property Handle: HWND read FHandle;
*)
    property HelpContext: Integer read GetHelpContext write SetHelpContext default 0;
(*
    property MainIcon: TTaskDialogIcon read FMainIcon write SetMainIcon default tdiInformation;
    property ProgressBar: TTaskDialogProgressBar read FProgressBar write FProgressBar;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
*)
    property TextMessage: string read GetText write SetText;
    property TitleMessage: string read GetTitle write SetTitle;
(*
    property URL: string read FURL;
    property VerificationText: string read FVerificationText write FVerificationText;
    property OnButtonClicked: TTaskDlgClickEvent read FOnButtonClicked write FOnButtonClicked;
    property OnDialogConstructed: TNotifyEvent read FOnDialogConstructed write FOnDialogConstructed;
    property OnDialogCreated: TNotifyEvent read FOnDialogCreated write FOnDialogCreated;
    property OnDialogDestroyed: TNotifyEvent read FOnDialogDestroyed write FOnDialogDestroyed;
    property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
    property OnHyperlinkClicked: TNotifyEvent read FOnHyperlinkClicked write FOnHyperlinkClicked;
    property OnNavigated: TNotifyEvent read FOnNavigated write FOnNavigated;
    property OnRadioButtonClicked: TNotifyEvent read FOnRadioButtonClicked write FOnRadioButtonClicked;
    property OnTimer: TTaskDlgTimerEvent read FOnTimer write FOnTimer;
    property OnVerificationClicked: TNotifyEvent read FOnVerificationClicked write FOnVerificationClicked;
*)
  protected
    procedure UpdateCustomIcons;
    procedure Loaded; override;
  public
    procedure SetDialogFont(const AFont: TFont);
    constructor Create(AOwner: TComponent); override;
  end;

procedure UseStyledDialogForm(const AActivate: Boolean);

implementation

{$R *.dfm}

uses
  System.Math
  , System.HelpIntfs
  , Winapi.ShellAPI
  , Vcl.StyledCmpMessages
  , System.Typinfo
  ;

var
  DialogLauncher: ITaskDialogLauncher;

procedure UseStyledDialogForm(const AActivate: Boolean);
begin
  if AActivate then
    RegisterCustomExecute( DialogLauncher)
  else
    UnregisterCustomExecute;
end;

{ TStyledTaskDialogForm }

procedure TStyledTaskDialogForm.SetButtons(const Value: TTaskDialogButtons);
var
  I: Integer;
  LTaskDialogButtonItem: TTaskDialogBaseButtonItem;
  LStyledButton: TStyledButton;
begin
  FButtons := Value;
  for I := FButtons.Count -1 downto 0 do
  begin
    LTaskDialogButtonItem := FButtons[I];
    case LTaskDialogButtonItem.ModalResult of
      mrYes: LStyledButton := YesButton;
      mrNo: LStyledButton := NoButton;
      mrOk: LStyledButton := OKButton;
      mrCancel: LStyledButton := CancelButton;
      mrAbort: LStyledButton := AbortButton;
      mrRetry: LStyledButton := RetryButton;
      mrIgnore: LStyledButton := IgnoreButton;
      mrAll: LStyledButton := AllButton;
      mrNoToAll: LStyledButton := NoToAllButton;
      mrYesToAll: LStyledButton := YesToAllButton;
      mrClose: LStyledButton := CloseButton;
    else
      LStyledButton := HelpButton;
    end;
    if Assigned(LStyledButton) then
    begin
      LStyledButton.Caption := LTaskDialogButtonItem.Caption;
      LStyledButton.Visible := True;
      LStyledButton.ModalResult := LTaskDialogButtonItem.ModalResult;
    end;
  end;
end;

procedure TStyledTaskDialogForm.FocusDefaultButton;
begin
  case DefaultButton of
    tcbOk: OKbutton.SetFocus;
    tcbYes: YesButton.SetFocus;
    tcbNo: NoButton.SetFocus;
    tcbCancel: CancelButton.SetFocus;
    tcbRetry: RetryButton.SetFocus;
    tcbClose: CloseButton.SetFocus;
  end;
end;

procedure TStyledTaskDialogForm.SetCommonButtons(
  const AValue: TTaskDialogCommonButtons);
begin
  FCommonButtons := AValue;
  CloseButton.Visible := (tcbClose in FCommonButtons);
  RetryButton.Visible := (tcbRetry in FCommonButtons);
  CancelButton.Visible := (tcbCancel in FCommonButtons);
  OKButton.Visible := (tcbOk in FCommonButtons);
  NoButton.Visible := (tcbNo in FCommonButtons);
  YesButton.Visible := (tcbYes in FCommonButtons);
end;

procedure TStyledTaskDialogForm.SetDialogFont(const AFont: TFont);
begin
  Self.Font.Assign(AFont);
  TitleLabel.Font.Name := Font.Name;
  TextLabel.Font.Name := Font.Name;
end;

procedure TStyledTaskDialogForm.SetHelpContext(const AValue: Integer);
begin
  inherited HelpContext := AValue;
end;

procedure TStyledTaskDialogForm.AdjustHeight;
const
  margins = 8;
var
  LBottomPanelHeight: Integer;
  LMinHeight, LCalcHeight: Integer;
begin
  if BottomPanel.Visible then
    LBottomPanelHeight := BottomPanel.Height
  else
    LBottomPanelHeight := 0;
  LCalcHeight :=
    AutoSizeLabel.Height + margins +
    TitleLabel.Height + margins +
    LBottomPanelHeight + margins +
    ButtonsPanel.Height + margins +
    margins + margins;
  LMinHeight := ImageList.Height +
    LBottomPanelHeight + margins +
    ButtonsPanel.Height +
    margins + margins;

  Constraints.MinHeight := LMinHeight +
    Height - ClientHeight;

  ClientHeight := Min(Self.Monitor.Height - 100,
    Max(LCalcHeight, LMinHeight));

  TextLabel.Font.Assign(AutoSizeLabel.Font);
  TextLabel.Height := AutoSizeLabel.Height;
  AutoSizeLabel.Visible := False;

  MessageScrollBox.VertScrollBar.Visible :=
    LCalcHeight > Constraints.MinHeight;
  //TextLabel.Visible := False;
end;

procedure TStyledTaskDialogForm.AdjustWidth;
var
  LFormWidth, I: Integer;
  LStyledButton: TStyledButton;
  LMargins: Integer;
begin
  LMargins := ButtonsPanel.Margins.Left;
  LFormWidth := LMargins;
  for I := 0 to ComponentCount -1 do
  begin
    if Components[I] is TStyledButton then
    begin
      LStyledButton := TStyledButton(Components[I]);
      if LStyledButton.Visible then
        LFormWidth := LFormWidth + LStyledButton.Width + LMargins + LMargins;
    end;
  end;
  LFormWidth := LFormWidth + LMargins;
  Width := Max(Width, LFormWidth);
end;

procedure TStyledTaskDialogForm.SetText(const AValue: string);
begin
  AutoSizeLabel.Caption := AValue;
  TextLabel.Caption := AValue;
end;

procedure TStyledTaskDialogForm.SetTitle(const AValue: string);
begin
  TitleLabel.Caption := AValue;
end;

procedure TStyledTaskDialogForm.LoadImage;
var
  LIconName: string;
  LIconIndex: Integer;
  LBitmap: TBitmap;
begin
  GetIconNameAndIndex(FDialogType, LIconName, LIconIndex);
  LBitmap := TBitmap.Create;
  try
    LBitmap.PixelFormat := pf32bit;
    ImageList.GetBitmap(lIconIndex, LBitmap);
    Image.Picture.Bitmap.Assign(LBitmap);
  finally
    LBitmap.free;
  end;
end;

procedure TStyledTaskDialogForm.ShowDialogForm;
begin
  //Initialize components based on ATaskDialog attributes
  Caption := FTaskDialog.Caption;
  HelpContext := FTaskDialog.HelpContext;
  CommonButtons := FTaskDialog.CommonButtons;
  Buttons := FTaskDialog.Buttons;
  DefaultButton := FTaskDialog.DefaultButton;
  LoadImage;

(*
    property Button: TTaskDialogButtonItem read FButton write FButton;
*)
(*
    property CustomFooterIcon: TIcon read FCustomFooterIcon write SetCustomFooterIcon;
    property CustomMainIcon: TIcon read FCustomMainIcon write SetCustomMainIcon;
    property ExpandButtonCaption: string read FExpandButtonCaption write FExpandButtonCaption;
    property Expanded: Boolean read FExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property FooterIcon: TTaskDialogIcon read FFooterIcon write SetFooterIcon default tdiNone;
    property FooterText: string read FFooterText write SetFooterText;
    property Handle: HWND read FHandle;
*)
(*
    property MainIcon: TTaskDialogIcon read FMainIcon write SetMainIcon default tdiInformation;
    property ProgressBar: TTaskDialogProgressBar read FProgressBar write FProgressBar;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
*)
    TextMessage := FTaskDialog.Text;
    TitleMessage :=  FTaskDialog.Title;
(*
    property URL: string read FURL;
    property VerificationText: string read FVerificationText write FVerificationText;
    property OnButtonClicked: TTaskDlgClickEvent read FOnButtonClicked write FOnButtonClicked;
    property OnDialogConstructed: TNotifyEvent read FOnDialogConstructed write FOnDialogConstructed;
    property OnDialogCreated: TNotifyEvent read FOnDialogCreated write FOnDialogCreated;
    property OnDialogDestroyed: TNotifyEvent read FOnDialogDestroyed write FOnDialogDestroyed;
    property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
    property OnHyperlinkClicked: TNotifyEvent read FOnHyperlinkClicked write FOnHyperlinkClicked;
    property OnNavigated: TNotifyEvent read FOnNavigated write FOnNavigated;
    property OnRadioButtonClicked: TNotifyEvent read FOnRadioButtonClicked write FOnRadioButtonClicked;
    property OnTimer: TTaskDlgTimerEvent read FOnTimer write FOnTimer;
    property OnVerificationClicked: TNotifyEvent read FOnVerificationClicked write FOnVerificationClicked;
*)
end;

procedure TStyledTaskDialogForm.TextLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(Self.Handle, 'open' , PChar(Link),
    nil, nil, SW_SHOW );
end;

procedure TStyledTaskDialogForm.UpdateCustomIcons;

  procedure GetAndSetIcon(const AType: TMsgDlgType;
    AColor: TColor);
  var
    LIcon: TIcon;
    LTypeName: string;
  begin
    LIcon := TIcon.Create;
    LTypeName := GetEnumName(TypeInfo(TMsgDlgType), Ord(AType));
    FreeAndNil(FCustomIcons[AType]);
    FCustomIcons[AType] := LIcon;
  end;

begin
  GetAndSetIcon(mtWarning, clYellow);
  GetAndSetIcon(mtError, clRed);
  GetAndSetIcon(mtInformation, clSkyBlue);
  GetAndSetIcon(mtConfirmation, clLime);
  GetAndSetIcon(mtCustom, clWindow);
(*
  if FRegistered then
    RegisterCustomIcons(FCustomIcons);
*)
end;

procedure TStyledTaskDialogForm.ButtonClick(Sender: TObject);
begin
  Close;
  self.ModalResult := (Sender as TStyledButton).ModalResult;
end;

constructor TStyledTaskDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  FCommonButtons := [tcbOk, tcbCancel];
  FDefaultButton := tcbOk;
end;

procedure TStyledTaskDialogForm.FormCreate(Sender: TObject);
//var
//  LRegion: hrgn;
begin
  BottomPanel.Visible := False;

//  LRegion := CreateRoundRectRgn(0, 0, Self.width, Self.height, 20, 20);
//  SetwindowRgn(handle, LRegion, true);
end;

procedure TStyledTaskDialogForm.FormDestroy(Sender: TObject);
begin
  FCustomIcons[mtWarning].Free;
  FCustomIcons[mtError].Free;
  FCustomIcons[mtInformation].Free;
  FCustomIcons[mtConfirmation].Free;
  FCustomIcons[mtCustom].Free;
end;

procedure TStyledTaskDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    CancelButton.Click
  else if key = VK_RETURN then
    OKButton.Click;
end;

procedure TStyledTaskDialogForm.PlayMessageDlgSound;
const
  Sounds: array [TMsgDlgType] of integer = (
    MB_ICONEXCLAMATION, MB_ICONHAND, MB_OK,
    MB_ICONQUESTION, MB_ICONASTERISK);
begin
  MessageBeep(Sounds[FDialogType]);
end;

procedure TStyledTaskDialogForm.FormShow(Sender: TObject);
begin
  ShowDialogForm;
  AdjustButtonsCaption;
  AdjustHeight;
  AdjustWidth;
  PlayMessageDlgSound;
  FocusDefaultButton;
end;

function TStyledTaskDialogForm.GetHelpContext: Integer;
begin
  Result := inherited HelpContext;
end;

procedure TStyledTaskDialogForm.GetIconNameAndIndex(
  ATaskDialog: TMsgDlgType; out AImageName: string; out AImageIndex: Integer);
const
  ImageNames: array[TMsgDlgType] of string =
    ('Warning', 'Error', 'Information', 'Confirmation', 'Shield');
begin
  AImageName := ImageNames[ATaskDialog];
  AImageIndex := Ord(ATaskDialog);
end;

function TStyledTaskDialogForm.GetText: string;
begin
  Result := TextLabel.Caption;
end;

function TStyledTaskDialogForm.GetTitle: string;
begin
  Result :=  TitleLabel.Caption;
end;

procedure TStyledTaskDialogForm.HelpButtonClick(Sender: TObject);
begin
  //TODO: implement call to Help
  Application.HelpContext(HelpContext);
end;

procedure TStyledTaskDialogForm.AdjustButtonsCaption;
begin
  YesButton.Caption := STR_YES;
  NoButton.Caption := STR_NO;
  OKButton.Caption := STR_OK;
  CancelButton.Caption := STR_CANCEL;
  HelpButton.Caption := STR_HELP;
  RetryButton.Caption := STR_RETRY;
end;

procedure TStyledTaskDialogForm.Loaded;
begin
  TextLabel.Align := alTop;

  TitleLabel.Font.Style := [TFontStyle.fsBold];
  TitleLabel.Font.Height := Round(TitleLabel.Font.Height * 1.4);
  TitleLabel.Font.Color := clHighlight;
  TextLabel.Font.Height := Round(TextLabel.Font.Height * 1.2);

  YesButton.ModalResult := mrYes;
  NoButton.ModalResult := mrNo;
  OKButton.ModalResult := mrOk;
  CancelButton.ModalResult := mrCancel;
  RetryButton.ModalResult := mrRetry;
  CloseButton.ModalResult := mrClose;

  inherited;
end;

function TTaskDialogLauncherHandler.DoExecute(ParentWnd: HWND;
  const ADialogType: TMsgDlgType;
  const ATaskDialog: TCustomTaskDialog): Boolean;
var
  LForm: TStyledTaskDialogForm;
  LFont: TFont;
begin
  LForm := TStyledTaskDialogForm.Create(nil);
  try
    LForm.FTaskDialog := ATaskDialog;
    LForm.FDialogType := ADialogType;
    LFont := GetDialogFont;
    if Assigned(LFont) then
      LForm.SetDialogFont(LFont)
    else
      LForm.SetDialogFont(Screen.MessageFont);
    LForm.ShowModal;
    ATaskDialog.ModalResult := LForm.ModalResult;
    Result := True;
  finally
    LForm.Free;
  end;
end;

initialization
  //Create handler for execute custom TaskDialog Form
  DialogLauncher := TTaskDialogLauncherHandler.Create;
  //Register the handler
  RegisterCustomExecute(DialogLauncher);

end.
