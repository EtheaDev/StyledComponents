{******************************************************************************}
{                                                                              }
{  StyledTaskDialogForm: a Task Dialog Form with StyleButtons                  }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
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

{$INCLUDE StyledComponents.inc}
{$WARN SYMBOL_PLATFORM OFF}
uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  System.UITypes,
  Vcl.StyledButton,
  Vcl.StyledTaskDialog,
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.ButtonStylesAttributes,
  Vcl.ColorButtonStyles,
  Vcl.ButtonGroup;

type
  TTaskDialogLauncherHandler = class(TInterfacedObject, ITaskDialogLauncher)
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TStyledTaskDialog;
      const ADialogBtnFamily: TStyledButtonFamily): Boolean;
  end;

  TStyledTaskDialogForm = class(TForm)
    FooterPanel: TPanel;
    ButtonsPanel: TPanel;
    ExpandedPanel: TPanel;
    VerificationPanel: TPanel;
    CommandLinksPanel: TPanel;
    CenterPanel: TPanel;
    ImagePanel: TPanel;
    FooterIconPanel: TPanel;
    IconContainer: TPanel;
    RadioGroupPanel: TPanel;
    MessageScrollBox: TScrollBox;
    TitleLabel: TLabel;
    TextLabel: TLinkLabel;
    AutoSizeLabel: TLabel;
    YesButton: TStyledButton;
    NoButton: TStyledButton;
    OKButton: TStyledButton;
    CancelButton: TStyledButton;
    AbortButton: TStyledButton;
    RetryButton: TStyledButton;
    IgnoreButton: TStyledButton;
    AllButton: TStyledButton;
    NoToAllButton: TStyledButton;
    YesToAllButton: TStyledButton;
    HelpButton: TStyledButton;
    CloseButton: TStyledButton;
    FooterTextLabel: TLinkLabel;
    VerificationCheckBox: TCheckBox;
    ExpandButton: TStyledButton;
    ExpandLabel: TLabel;
    ProgressBarPanel: TPanel;
    InternalProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TextLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VerificationCheckBoxClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
    procedure ExpandButtonClick(Sender: TObject);
  private
    FFocusedButton: TStyledButton;
    FCustomIcons: TStyledDialogIcons;
    FTaskDialog: TCustomTaskDialog;
    FDialogType: TMsgDlgType;
    FButtonsWidth: Integer;
    FButtonsHeight: Integer;
    FDialogBtnFamily: TStyledButtonFamily;
    FCommonButtons: TTaskDialogCommonButtons;
    FDefaultButton: TTaskDialogCommonButton;
    FUseMessageDefaultButton: Boolean;
    FMessageDefaultButton: TMsgDlgBtn;
    FButtons: TTaskDialogButtons;
    FFooterIcon: TTaskDialogIcon;
    FCustomMainIcon: TIcon;
    FCustomFooterIcon: TIcon;
    FMainIcon: TTaskDialogIcon;
    FOnTimer: TTaskDlgTimerEvent;
    FRadioButtons: TTaskDialogButtons;
    FAutoClick: Boolean;
    FAutoClickDelay: Integer;
    FRadioButton: TTaskDialogRadioButtonItem;
    FProgressBar: TTaskDialogProgressBar;
    FFlags: TTaskDialogFlags;
    FExpandButtonCaption: string;
    FText: string;
    FExpandedText: string;
    FTaskDialogExpanded: TNotifyEvent;
    FTimer: TTimer;
    FTickCount: Cardinal;
    //procedure GetIconNameAndIndex(ATaskDialog: TMsgDlgType;
    //  out AImageName: string; out AImageIndex: Integer); overload;
    procedure TaskDialogExpanded(Sender: TObject);
    procedure GetIconNameAndIndex(ATaskDialogIcon: TTaskDialogIcon;
      out AImageName: string; out AImageIndex: Integer); overload;
    procedure ShowDialogForm;
    procedure SetHelpContext(const AValue: Integer);
    function GetHelpContext: Integer;
    function GetText: string;
    function GetTitle: string;
    procedure SetText(const AValue: string);
    procedure SetTitle(const AValue: string);
    procedure AdjustHeight;
    procedure AdjustWidth;
    procedure AdjustControlsTopPos;
    procedure AdjustButtonsCaption;
    procedure SetButtons(const AValue: TTaskDialogButtons);
    procedure PlayMessageDlgSound;
    procedure FocusDefaultButton;
    procedure LoadDialogImage;
    procedure SetFooterText(const AValue: string);
    function GetFooterText: string;
    procedure SetVerificationText(const AValue: string);
    procedure SetExpandedText(const AValue: string);
    function GetVerificationText: string;
    function GetFocusedButton: TStyledButton;
    procedure InitDlgButtonsWithFamily(const AFamily: TStyledButtonFamily);
    procedure UpdateButtonsVisibility;
    procedure UpdateButtonsSize;
    procedure SetFooterIcon(const AValue: TTaskDialogIcon);
    procedure SetCustomMainIcon(const AValue: TIcon);
    procedure SetCustomFooterIcon(const AValue: TIcon);
    procedure SetMainIcon(const AValue: TTaskDialogIcon);
    procedure AddCustomButtons(const AButtons: TTaskDialogButtons);
    procedure SetMainIconSize(const AValue: Integer);
    function GetMainIconSize: Integer;
    procedure SetRadioButtons(const AValue: TTaskDialogButtons);
    function UsingCommandLinks: Boolean;
    procedure SetFlags(const AValue: TTaskDialogFlags);
    procedure SetExpandButtonCaption(const Value: string);
    function GetExpanded: Boolean;
    procedure CalcMessageText(const AExpanded: Boolean);
    procedure SetProgressBar(const AValue: TTaskDialogProgressBar);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure TimerEvent(Sender: TObject);
  	function GetScaleFactor: Single; virtual;
    class function CanUseAnimations: Boolean; virtual; abstract;
    function GetButtonsHeight: Integer; virtual;
    function GetButtonsWidth: Integer; virtual;
    procedure SetButtonsHeight(const AValue: Integer); virtual;
    procedure SetButtonsWidth(const AValue: Integer); virtual;
    function TaskDialogIconToImageIndex(const ATaskDialogIcon: TTaskDialogIcon): Integer;
    function TaskDialogIconToImageName(const ATaskDialogIcon: TTaskDialogIcon): string;
    procedure Loaded; override;
    procedure LoadImage(const AImageIndex: TImageIndex;
      AImageName: string); virtual; abstract;
    procedure LoadCustomMainIcon(const AIcon: TIcon;
      const ATaskDialogIcon: TTaskDialogIcon); virtual;
    procedure LoadCustomFooterIcon(const AIcon: TIcon;
      const ATaskDialogIcon: TTaskDialogIcon); virtual;
    procedure DefaultDialogSize(out AClientWidth, AClientHeight, AImageSize: Integer); virtual;
    function GetDefaultButton(const ADefaultButton: TTaskDialogCommonButton): TStyledButton; virtual;
    property TaskDialog: TCustomTaskDialog read FTaskDialog;
  public
    function ModalResultToCommonButton(const AModalResult: TModalResult;
      out ACommonButton: TTaskDialogCommonButton): Boolean;
    function FindButton(const AModalResult: TModalResult): TStyledButton;
    procedure SetDialogFont(const AFont: TFont); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetPosition(const X, Y: Integer); virtual;
    property AutoClick: Boolean read FAutoClick write FAutoClick default False;
    property AutoClickDelay: Integer read FAutoClickDelay write FAutoClickDelay default DEFAULT_AUTOCLICK_DELAY;
    property ButtonsWidth: Integer read GetButtonsWidth write SetButtonsWidth;
    property ButtonsHeight: Integer read GetButtonsHeight write SetButtonsHeight;
    property Buttons: TTaskDialogButtons read FButtons write SetButtons;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
    property CommonButtons: TTaskDialogCommonButtons read FCommonButtons write FCommonButtons default [tcbOk, tcbCancel];
    property DefaultButton: TTaskDialogCommonButton read FDefaultButton write FDefaultButton default TTaskDialogCommonButton.tcbOk;
    property MessageDefaultButton: TMsgDlgBtn read FMessageDefaultButton write FMessageDefaultButton default TMsgDlgBtn.mbOK;
    property ExpandButtonCaption: string read FExpandButtonCaption write SetExpandButtonCaption;
    property Expanded: Boolean read GetExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property ProgressBar: TTaskDialogProgressBar read FProgressBar write SetProgressBar;
(*
    property URL: string read FURL;
*)
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property MainIcon: TTaskDialogIcon read FMainIcon write SetMainIcon default tdiInformation;
    property CustomMainIcon: TIcon read FCustomMainIcon write SetCustomMainIcon;
    property MainIconSize: Integer read GetMainIconSize write SetMainIconSize default DEFAULT_MAIN_ICON_SIZE;
    property CustomFooterIcon: TIcon read FCustomFooterIcon write SetCustomFooterIcon;
    property FooterIcon: TTaskDialogIcon read FFooterIcon write SetFooterIcon default tdiNone;
    property FooterText: string read GetFooterText write SetFooterText;
    property VerificationText: string read GetVerificationText write SetVerificationText;
    property HelpContext: Integer read GetHelpContext write SetHelpContext default 0;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property TextMessage: string read GetText write SetText;
    property TitleMessage: string read GetTitle write SetTitle;
    property OnTimer: TTaskDlgTimerEvent read FOnTimer write FOnTimer;
  end;

  TStyledTaskDialogFormClass = class of TStyledTaskDialogForm;

/// <summary>
///  To activate or deactivate use of Custom Task Dialog
/// </summary>
procedure UseStyledDialogForm(const AActivate: Boolean);

/// <summary>
///  Register the custom StyledTaskDialog passing the Form Class
///  The Form must inherits from TStyledTaskDialogForm
/// </summary>
procedure RegisterTaskDialogFormClass(
  AFormClass: TStyledTaskDialogFormClass);

/// <summary>
///  Unregister the custom StyledTaskDialog to use Standard Task Dialog
/// </summary>
procedure UnRegisterTaskDialogFormClass(
  AFormClass: TStyledTaskDialogFormClass);

implementation

{$R *.dfm}

uses
  System.Math
  , Vcl.Themes
  , System.HelpIntfs
  , Winapi.ShellAPI
  , Winapi.CommCtrl
  , Winapi.MultiMon
  , Vcl.StyledCmpMessages
  , System.Typinfo
  ;

var
  _DialogLauncher: ITaskDialogLauncher;
  _AnimatedTaskDialogFormClass: TStyledTaskDialogFormClass;
  _TaskDialogFormClass: TStyledTaskDialogFormClass;
  _DlgButtonClasses: TButtonClasses;
  _DialogPosition: Vcl.Forms.TPosition;

procedure UnRegisterTaskDialogFormClass(
  AFormClass: TStyledTaskDialogFormClass);
begin
  //Unregister handle
  _DialogLauncher := nil;
  if AFormClass = _AnimatedTaskDialogFormClass then
    _AnimatedTaskDialogFormClass := nil;
  if AFormClass = _TaskDialogFormClass then
    _TaskDialogFormClass := nil;
end;

procedure RegisterTaskDialogFormClass(
  AFormClass: TStyledTaskDialogFormClass);
begin
  //Create handler for execute custom TaskDialog Form
  _DialogLauncher := TTaskDialogLauncherHandler.Create;
  if AFormClass.CanUseAnimations then
    _AnimatedTaskDialogFormClass := AFormClass
  else
    _TaskDialogFormClass := AFormClass;
  UseStyledDialogForm(True);
end;

procedure UseStyledDialogForm(const AActivate: Boolean);
begin
  if AActivate then
    RegisterCustomExecute(_DialogLauncher)
  else
    UnregisterCustomExecute;
end;

{ TStyledTaskDialogForm }

procedure TStyledTaskDialogForm.SetRadioButtons(const AValue: TTaskDialogButtons);
var
  I: Integer;
  LTaskDialogRadioButtonItem: TTaskDialogRadioButtonItem;
  LRadioButton, LLastButton, LFirstButton: TRadioButton;
  LHeight: Integer;
  LDefaultAssigned: Boolean;
begin
  LHeight := RadioGroupPanel.Height;
  FRadioButtons := AValue;
  LLastButton := nil;
  LFirstButton := nil;
  LDefaultAssigned := False;
  for I := 0 to FRadioButtons.Count -1 do
  begin
    LTaskDialogRadioButtonItem := FRadioButtons[I] as TTaskDialogRadioButtonItem;
    LRadioButton := TRadioButton.Create(Self);
    if I = 0 then
      LFirstButton := LRadioButton;
    LRadioButton.Tag := 100+I;
    LRadioButton.Caption := LTaskDialogRadioButtonItem.Caption;
    LRadioButton.Parent := RadioGroupPanel;
    LRadioButton.Align := alTop;
    LRadioButton.Height := LHeight;
    if Assigned(LLastButton) then
      LRadioButton.Top := LLastButton.Top + LLastButton.Height;
    LRadioButton.OnClick := RadioButtonClick;
    if LTaskDialogRadioButtonItem.Default then
    begin
      LRadioButton.Checked := True;
      LDefaultAssigned := True;
    end;
    RadioGroupPanel.Visible := True;
    RadioGroupPanel.Height := LHeight * (I+1);
    LLastButton := LRadioButton;
  end;
  if (FRadioButtons.Count > 0) then
  begin
    RadioGroupPanel.Height := RadioGroupPanel.Height + (LHeight div 3);
    if not LDefaultAssigned then
      LFirstButton.Checked := True;
  end;
end;

procedure TStyledTaskDialogForm.SetButtons(const AValue: TTaskDialogButtons);
var
  I: Integer;
  LTaskDialogButtonItem: TTaskDialogBaseButtonItem;
  LStyledButton: TStyledButton;
begin
  FButtons := AValue;
  for I := FButtons.Count -1 downto 0 do
  begin
    LTaskDialogButtonItem := FButtons[I];
    if Assigned(LTaskDialogButtonItem) then
    begin
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
        mrHelp: LStyledButton := HelpButton;
      else
        begin
          LStyledButton := nil;
        end;
      end;
      if Assigned(LStyledButton) then
      begin
        TabOrder := LStyledButton.TabOrder -1;
        if LTaskDialogButtonItem.Default then
          FFocusedButton := LStyledButton;
      end;
    end;
  end;
end;

procedure TStyledTaskDialogForm.SetButtonsHeight(const AValue: Integer);
begin
  if FButtonsHeight <> AValue then
  begin
    FButtonsHeight := AValue;
    UpdateButtonsSize;
  end;
end;

procedure TStyledTaskDialogForm.SetButtonsWidth(const AValue: Integer);
begin
  if FButtonsHeight <> AValue then
  begin
    FButtonsHeight := AValue;
    UpdateButtonsSize;
  end;
end;

procedure TStyledTaskDialogForm.SetCustomMainIcon(const AValue: TIcon);
begin
  if FCustomMainIcon <> AValue then
  begin
    FCustomMainIcon := AValue;
    LoadCustomMainIcon(FCustomMainIcon, FMainIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetCustomFooterIcon(const AValue: TIcon);
begin
  if FCustomFooterIcon <> AValue then
  begin
    FCustomMainIcon := AValue;
    LoadCustomFooterIcon(FCustomFooterIcon, FfooterIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetFlags(const AValue: TTaskDialogFlags);
begin
  if FFlags <> AValue then
  begin
    FFlags := AValue;
    FTaskDialog.Flags := AValue;
    ProgressBarPanel.Visible := (tfShowProgressBar in Flags) or (tfShowMarqueeProgressBar in Flags);
    if tfShowMarqueeProgressBar in Flags then
      InternalProgressBar.Style := pbstMarquee;
  end;
end;

procedure TStyledTaskDialogForm.SetMainIcon(const AValue: TTaskDialogIcon);
begin
  if FMainIcon <> AValue then
  begin
    FMainIcon := AValue;
    LoadCustomMainIcon(FCustomMainIcon, FMainIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetMainIconSize(const AValue: Integer);
begin
  if AValue <> ImagePanel.Width then
  begin
    ImagePanel.Width := AValue;
    IconContainer.Height := AValue;
  end;
end;

procedure TStyledTaskDialogForm.SetProgressBar(
  const AValue: TTaskDialogProgressBar);
begin
  FProgressBar := AValue;
  InternalProgressBar.Min := FProgressBar.Min;
  InternalProgressBar.Max := FProgressBar.Max;
  InternalProgressBar.Position := FProgressBar.Position;
  InternalProgressBar.MarqueeInterval := FProgressBar.MarqueeSpeed;
  InternalProgressBar.State := FProgressBar.State;
end;

procedure TStyledTaskDialogForm.SetFooterIcon(const AValue: TTaskDialogIcon);
begin
  if FFooterIcon <> AValue then
  begin
    FFooterIcon := AValue;
    LoadCustomFooterIcon(FCustomFooterIcon, FFooterIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetFooterText(const AValue: string);
var
  LRows: TStringList;
begin
  if Trim(AValue) <> '' then
  begin
    LRows := TStringList.Create;
    try
      LRows.Text := AValue;
      FooterTextLabel.Caption := AValue;
      if LRows.Count > 1 then
        FooterPanel.Height := FooterPanel.Height * LRows.Count
      else
        FooterPanel.Height := 32;
      FooterPanel.Visible := True;
    finally
      LRows.Free;
    end;
  end
  else
    FooterPanel.Visible := False;
end;

function TStyledTaskDialogForm.GetButtonsHeight: Integer;
begin
  //Assuming all buttons have the same Height
  Result := YesButton.Height;
end;

function TStyledTaskDialogForm.GetButtonsWidth: Integer;
begin
  //Assuming all buttons have the same Width
  Result := YesButton.Width;
end;

function TStyledTaskDialogForm.GetDefaultButton(
  const ADefaultButton :TTaskDialogCommonButton): TStyledButton;
begin
  if FUseMessageDefaultButton then
  begin
    case FMessageDefaultButton of
      mbYes: Result := YesButton;
      mbNo: Result := NoButton;
      mbOK: Result := OKButton;
      mbCancel: Result := CancelButton;
      mbAbort: Result := AbortButton;
      mbRetry: Result := RetryButton;
      mbIgnore: Result := IgnoreButton;
      mbAll: Result := AllButton;
      mbNoToAll: Result := NoToAllButton;
      mbYesToAll: Result := YesToAllButton;
      mbHelp: Result := HelpButton;
      mbClose: Result := CloseButton;
    else
      Result := nil;
    end;
  end
  else
  begin
    case ADefaultButton of
      tcbOk: Result := OKButton;
      tcbYes: Result := YesButton;
      tcbNo: Result := NoButton;
      tcbCancel: Result := CancelButton;
      tcbRetry: Result := RetryButton;
      tcbClose: Result := CloseButton;
    else
      Result := nil;
    end;
  end;
end;

function TStyledTaskDialogForm.GetExpanded: Boolean;
begin
  Result := FTaskDialog.Expanded;
end;

procedure TStyledTaskDialogForm.FocusDefaultButton;
var
  LButton: TStyledButton;
begin
  LButton := GetDefaultButton(DefaultButton);
  if not Assigned(LButton) then
    LButton := FFocusedButton;
  if Assigned(LButton) then
  begin
    if LButton.CanFocus and Self.Visible then
    begin
      LButton.SetFocus;
      LButton.AutoClick := FAutoClick;
      LButton.AutoClickDelay := FAutoClickDelay;
    end;
  end;
end;

procedure TStyledTaskDialogForm.SetDialogFont(const AFont: TFont);
begin
  Self.Font.Assign(AFont);
  TextLabel.Font.Name := Font.Name;
  TextLabel.Font.Size := AFont.Size;
  //TitleLabel font attributes
  if not StyleServices.Enabled or StyleServices.IsSystemStyle then
    TitleLabel.Font.Color := clHighlight
  else
    TextLabel.Font.Color := StyleServices.GetSystemColor(clHighlight);
  TitleLabel.Font.Style := [TFontStyle.fsBold];
  TitleLabel.Font.Name := Font.Name;
  TitleLabel.Font.Height := Round(AFont.Height * 1.4);
end;

procedure TStyledTaskDialogForm.SetHelpContext(const AValue: Integer);
begin
  inherited HelpContext := AValue;
  HelpButton.HelpContext := AValue;
end;

procedure TStyledTaskDialogForm.AdjustHeight;
var
  LTitleHeight: Integer;
  LRadioGroupPanelHeight: Integer;
  LCommandLinksPanelHeight: Integer;
  LVerificationPanelHeight: Integer;
  LProgressBarPanelHeight: Integer;
  LExpandedPanelHeight: Integer;
  LButtonsPanelHeight: Integer;
  LFooterPanelHeight: Integer;
  LMinHeight, LCalcHeight: Integer;
  LImageSize, LMessageHeight, LWidth, LHeight, LMargins: Integer;
begin
  LMargins := CenterPanel.Margins.Top * 2;
  DefaultDialogSize(LWidth, LHeight, LImageSize);

  //Height of Buttons
  if FButtonsHeight <> DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT then
    ButtonsPanel.Height := FButtonsHeight + LMargins + 2;

  //Calculate Height of Form, based on Visible Components
  if TitleLabel.Caption <> '' then
  begin
    LTitleHeight := TitleLabel.Height + LMargins;
  end
  else
  begin
    LTitleHeight := 0;
    TitleLabel.Visible := False;
  end;

  if RadioGroupPanel.Visible then
    LRadioGroupPanelHeight := RadioGroupPanel.Height + LMargins
  else
    LRadioGroupPanelHeight := 0;

  if FooterPanel.Visible then
    LFooterPanelHeight := FooterPanel.Height + LMargins
  else
    LFooterPanelHeight := 0;

  if VerificationPanel.Visible then
    LVerificationPanelHeight := VerificationPanel.Height + LMargins
  else
    LVerificationPanelHeight := 0;

  if ProgressBarPanel.Visible then
    LProgressBarPanelHeight := ProgressBarPanel.Height + LMargins
  else
    LProgressBarPanelHeight := 0;

  if ExpandedPanel.Visible then
    LExpandedPanelHeight := ExpandedPanel.Height + LMargins
  else
    LExpandedPanelHeight := 0;

  if CommandLinksPanel.Visible then
    LCommandLinksPanelHeight := CommandLinksPanel.Height + (LMargins * 2)
  else
    LCommandLinksPanelHeight := 0;

  if ButtonsPanel.Visible then
    LButtonsPanelHeight := ButtonsPanel.Height + LMargins
  else
    LButtonsPanelHeight := 0;

  LMessageHeight := AutoSizeLabel.Height + LMargins;

  //Actual Height based on size of Message
  LCalcHeight :=
    LTitleHeight +
    LMessageHeight +
    LExpandedPanelHeight +
    LRadioGroupPanelHeight +
    LFooterPanelHeight +
    LVerificationPanelHeight +
    LProgressBarPanelHeight +
    LButtonsPanelHeight +
    LCommandLinksPanelHeight;

  //Minumum Height based on size of Image
  LMinHeight :=
    LTitleHeight +
    LImageSize + LMargins +
    LExpandedPanelHeight +
    LRadioGroupPanelHeight +
    LFooterPanelHeight +
    LVerificationPanelHeight +
    LProgressBarPanelHeight +
    LButtonsPanelHeight +
    LCommandLinksPanelHeight;

  Constraints.MinHeight := LMinHeight +
    Height - ClientHeight;

  LHeight := Min(Self.Monitor.Height - 100,
    Max(LCalcHeight, LMinHeight));

  ClientHeight := LHeight;

  TextLabel.Font.Assign(AutoSizeLabel.Font);
  TextLabel.Height := LMessageHeight;

  if LCalcHeight > LHeight then
  begin
    MessageScrollBox.VertScrollBar.Visible := True;
    {$IFDEF D11+}
    //You need Delphi 11 Update 3 to compile this row or remove it!
    MessageScrollBox.UseWheelForScrolling := True;
    {$ENDIF}
    MessageScrollBox.VertScrollBar.Range := LMessageHeight + LTitleHeight + 100;
    TextLabel.Align := alClient;
  end
  else
    MessageScrollBox.VertScrollBar.Visible := False;

  AutoSizeLabel.Visible := False;
end;

procedure TStyledTaskDialogForm.AdjustWidth;
var
  LFormWidth, I: Integer;
  LStyledButton: TStyledButton;
  LMargins: Integer;
  LImageSize, LWidth, LHeight: Integer;
begin
  DefaultDialogSize(LWidth, LHeight, LImageSize);
  LMargins := ButtonsPanel.Margins.Left;
  LFormWidth := LMargins;
  if ButtonsPanel.Visible then
  begin
    for I := 0 to ComponentCount - 1 do
    begin
      if Components[I] is TStyledButton then
      begin
        LStyledButton := TStyledButton(Components[I]);
        if LStyledButton.Visible and (LStyledButton.Align = alRight) then
          LFormWidth := LFormWidth + LStyledButton.Width + LMargins + LMargins;
      end;
    end;
  end;
  LFormWidth := LFormWidth + LMargins;
  ClientWidth := Max(LWidth, LFormWidth);
  //Resize Image based on Scale Factor
  ImagePanel.SetBounds(ImagePanel.Left, ImagePanel.Top,
    LImageSize, LImageSize);
  IconContainer.SetBounds(IconContainer.Left, IconContainer.Top,
    LImageSize, LImageSize);
end;

procedure TStyledTaskDialogForm.CalcMessageText(const AExpanded: Boolean);
var
  LMessage: string;
begin
  if not AExpanded then
  begin
    ExpandButton.Caption := '+';
    if ExpandButtonCaption = '' then
      ExpandLabel.Caption := SShowDetails;
  end
  else
  begin
    ExpandButton.Caption := '-';
    if ExpandButtonCaption = '' then
      ExpandLabel.Caption := SHideDetails;
  end;
  LMessage := FText;
  if AExpanded and (FExpandedText <> '') then
    LMessage := LMessage + sLineBreak + sLineBreak + FExpandedText;
  if TextLabel.Caption <> LMessage then
  begin
    AutoSizeLabel.Caption := ClearHRefs(LMessage);
    TextLabel.Caption := LMessage;
  end;
end;

procedure TStyledTaskDialogForm.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    CalcMessageText(Expanded);
  end;
end;

procedure TStyledTaskDialogForm.SetTitle(const AValue: string);
begin
  if TitleLabel.Caption <> AValue then
  begin
    TitleLabel.Caption :=  AValue;
  end;
end;

procedure TStyledTaskDialogForm.SetVerificationText(const AValue: string);
var
  LRows: TStringList;
begin
  if Trim(AValue) <> '' then
  begin
    LRows := TStringList.Create;
    try
      LRows.Text := AValue;
      VerificationCheckBox.Caption := AValue;
      if LRows.Count > 1 then
        VerificationPanel.Height := VerificationPanel.Height * LRows.Count;
      VerificationPanel.Visible := True;
    finally
      LRows.Free;
    end;
  end
  else
    VerificationPanel.Visible := False;
end;

procedure TStyledTaskDialogForm.SetExpandButtonCaption(const Value: string);
begin
  FExpandButtonCaption := Value;
end;

procedure TStyledTaskDialogForm.SetExpandedText(const AValue: string);
begin
  if Trim(AValue) <> FExpandedText then
  begin
    FExpandedText := AValue;
    ExpandedPanel.Visible := FExpandedText <> '';
    CalcMessageText(Expanded);
  end;
end;

procedure TStyledTaskDialogForm.LoadCustomFooterIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
begin
  //In descendand form implements this method
  ;
end;

procedure TStyledTaskDialogForm.LoadCustomMainIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
begin
  //In descendand form implements this method
  ;
end;

procedure TStyledTaskDialogForm.LoadDialogImage;
var
  LIconName: string;
  LIconIndex: Integer;
  LMargins: Integer;
begin
  if (tfUseHiconMain in FTaskDialog.Flags) and Assigned(FTaskDialog.CustomMainIcon) then
  begin
    LoadCustomMainIcon(FTaskDialog.CustomMainIcon, FTaskDialog.MainIcon);
  end
  else
  begin
    GetIconNameAndIndex(FMainIcon, LIconName, LIconIndex);
    LoadImage(LIconIndex, LIconName);
  end;
  LMargins := CenterPanel.Margins.Left + CenterPanel.Margins.Right;
  if ImagePanel.Visible then
    Inc(LMargins, ImagePanel.Width);
  RadioGroupPanel.Margins.Left := LMargins;
  CommandLinksPanel.Margins.Left := LMargins;
  ProgressBarPanel.Margins.Left := LMargins;
end;

function TStyledTaskDialogForm.FindButton(const AModalResult: TModalResult): TStyledButton;
var
  I: Integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if Components[I] is TStyledButton then
    begin
      Result := TStyledButton(Components[I]);
      if Result.ModalResult = AModalResult then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TStyledTaskDialogForm.UpdateButtonsSize;

  procedure UpdateButtonSize(const AButton: TStyledButton);
  begin
    AButton.Width := Round(FButtonsWidth * GetScaleFactor);
  end;

begin
  UpdateButtonSize(YesButton);
  UpdateButtonSize(NoButton);
  UpdateButtonSize(OKButton);
  UpdateButtonSize(CancelButton);
  UpdateButtonSize(RetryButton);
  UpdateButtonSize(CloseButton);
  UpdateButtonSize(AbortButton);
  UpdateButtonSize(IgnoreButton);
  UpdateButtonSize(AllButton);
  UpdateButtonSize(NoToAllButton);
  UpdateButtonSize(YesToAllButton);
  UpdateButtonSize(HelpButton);
end;

procedure TStyledTaskDialogForm.UpdateButtonsVisibility;

  function IsButtonVisible(const AButton: TStyledButton): Boolean;
  var
    I: Integer;
    LTaskDialogButtonItem: TTaskDialogBaseButtonItem;
  begin
    Result := False;
    for I := FButtons.Count - 1 downto 0 do
    begin
      LTaskDialogButtonItem := FButtons[I];
      if Assigned(LTaskDialogButtonItem) and
        (AButton.ModalResult = LTaskDialogButtonItem.ModalResult) then
      begin
        Result := True;
        break;
      end;
    end;
  end;

begin
  if not UsingCommandLinks then
  begin
    YesButton.Visible := (tcbYes in FCommonButtons) or IsButtonVisible(YesButton);
    NoButton.Visible := (tcbNo in FCommonButtons) or IsButtonVisible(NoButton);
    OKButton.Visible := (tcbOk in FCommonButtons) or IsButtonVisible(OKButton);
    CancelButton.Visible := (tcbCancel in FCommonButtons) or IsButtonVisible(CancelButton);
    RetryButton.Visible := (tcbRetry in FCommonButtons) or IsButtonVisible(RetryButton);
    CloseButton.Visible := (tcbClose in FCommonButtons) or IsButtonVisible(CloseButton);
    AbortButton.Visible := IsButtonVisible(AbortButton);
    IgnoreButton.Visible := IsButtonVisible(IgnoreButton);
    AllButton.Visible := IsButtonVisible(AllButton);
    NoToAllButton.Visible := IsButtonVisible(NoToAllButton);
    YesToAllButton.Visible := IsButtonVisible(YesToAllButton);
    HelpButton.Visible := IsButtonVisible(HelpButton);
  end
  else
  begin
    YesButton.Visible := (tcbYes in FCommonButtons);
    NoButton.Visible := (tcbNo in FCommonButtons);
    OKButton.Visible := (tcbOk in FCommonButtons);
    CancelButton.Visible := (tcbCancel in FCommonButtons);
    RetryButton.Visible := (tcbRetry in FCommonButtons);
    CloseButton.Visible := (tcbClose in FCommonButtons);
    AbortButton.Visible := False;
    IgnoreButton.Visible := False;
    AllButton.Visible := False;
    NoToAllButton.Visible := False;
    YesToAllButton.Visible := False;
    HelpButton.Visible := False;
  end;
end;

function TStyledTaskDialogForm.UsingCommandLinks: Boolean;
begin
  Result := (tfUseCommandLinks in FTaskDialog.Flags) and
    (FTaskDialog.Title <> '');
end;

procedure TStyledTaskDialogForm.AddCustomButtons(const AButtons: TTaskDialogButtons);
var
  LButtonItem: TTaskDialogButtonItem;
  I: Integer;
  LLastButton, LStyledButton: TStyledButton;
  LUsingCommandLinks: Boolean;
  LCommandLinkHeight: Integer;
begin
  LLastButton := nil;
  LCommandLinkHeight := Max(CommandLinksPanel.Height, ButtonsHeight);
  LUsingCommandLinks := UsingCommandLinks;
  if LUsingCommandLinks then
  begin
    CommandLinksPanel.Visible := True;
    if CommonButtons = [] then
    begin
      ButtonsPanel.Visible := False;
      ButtonsPanel.Height := 0;
    end
    else
      ButtonsPanel.Visible := True;
  end;

  CommandLinksPanel.Height := 0;
  for I := 0 to AButtons.Count - 1 do //downto 0 do
  begin
    LButtonItem := AButtons.Items[I] as TTaskDialogButtonItem;
    // Find if the Button is already present in the Form
    // for CommandLinks the buttons must be always created
    LStyledButton := FindButton(LButtonItem.ModalResult);
    if not Assigned(LStyledButton) or LUsingCommandLinks then
    begin
      LStyledButton := TStyledButton.Create(Self);
      LStyledButton.OnClick := ButtonClick;
      LStyledButton.SetButtonStyle(FDialogBtnFamily, LButtonItem.ModalResult);
    end
    else
    begin
      // Show the Button if not using Command Links
      LStyledButton.Visible := Assigned(LStyledButton) and not LUsingCommandLinks;
    end;
    LStyledButton.Caption := LButtonItem.Caption;
    LStyledButton.Default := LButtonItem.Default;
    LStyledButton.ElevationRequired := LButtonItem.ElevationRequired;
    LStyledButton.Enabled := LButtonItem.Enabled;
    if LUsingCommandLinks then
    begin
      LStyledButton.Parent := CommandLinksPanel;
      LStyledButton.TabOrder := 0;
      LStyledButton.AlignWithMargins := True;
      LStyledButton.Margins.Top := 1;
      LStyledButton.Margins.Bottom := 1;
      LStyledButton.Align := alBottom;
      LStyledButton.Style := bsCommandLink;
      LStyledButton.CommandLinkHint := LButtonItem.CommandLinkHint;
      LStyledButton.Height := LCommandLinkHeight - 2;
      CommandLinksPanel.Height := CommandLinksPanel.Height + LCommandLinkHeight;
      if LButtonItem.Default then
      begin
        LStyledButton.Default := True;
        Self.ActiveControl := LStyledButton;
        FFocusedButton := LStyledButton;
      end;
      if Assigned(LLastButton) then
      begin
        LStyledButton.TabOrder := LLastButton.TabOrder +1;
        LStyledButton.Top := LLastButton.Top + LLastButton.Height;
      end
      else
      begin
        LStyledButton.TabOrder := CloseButton.TabOrder +1;
      end;
      LLastButton := LStyledButton;
    end
    else
    begin
      LStyledButton.Parent := ButtonsPanel;
      LStyledButton.Width := ButtonsWidth;
      LStyledButton.Align := alRight;
      LStyledButton.AlignWithMargins := True;
      LStyledButton.Margins.Assign(YesButton.Margins);
      if Assigned(LLastButton) then
      begin
        LStyledButton.TabOrder := LLastButton.TabOrder +1;
        LStyledButton.Left := LLastButton.Left + LLastButton.Width;
      end
      else
      begin
        LStyledButton.TabOrder := CloseButton.TabOrder +1;
        LStyledButton.Left := YesButton.Left - ButtonsWidth;
      end;
      LLastButton := LStyledButton;
    end;
  end;
  //Set Focus to Button assigning ActiveControl
  if (Self.ActiveControl = nil) and Assigned(LLastButton) and (LLastButton.CanFocus) then
  begin
    Self.ActiveControl := LLastButton;
    FFocusedButton := LLastButton;
  end;
end;

procedure TStyledTaskDialogForm.AdjustControlsTopPos;
var
  LTop: Integer;

  procedure AddHeight(const AControl: TControl);
  begin
    if AControl.Visible then
      Inc(LTop, AControl.Height);
  end;

  procedure SetTop(const AControl: TControl);
  begin
    if AControl.Visible then
    begin
      AControl.Top := LTop+1;
      Inc(LTop, AControl.Height);
//      if AControl.AlignWithMargins then
//        Inc(LTop, AControl.Margins.Top + AControl.Margins.Bottom);
    end;
  end;

begin
  LTop := 0;
  //Calculating Top
  AddHeight(ExpandedPanel);
  AddHeight(RadioGroupPanel);
  AddHeight(CommandLinksPanel);
  AddHeight(ButtonsPanel);
  AddHeight(VerificationPanel);
  AddHeight(FooterPanel);
  //Calculating Top as difference
  LTop := ClientHeight - LTop;

  //Setting Top for components starting from Top Visible Component
  SetTop(ExpandedPanel);
  SetTop(RadioGroupPanel);
  SetTop(CommandLinksPanel);
  SetTop(ButtonsPanel);
  SetTop(VerificationPanel);
  SetTop(FooterPanel);
end;

procedure TStyledTaskDialogForm.ShowDialogForm;
var
  LStyledDialog: TStyledTaskDialog;
begin
  //Initialize components based on ATaskDialog attributes
  FTaskDialogExpanded := FTaskDialog.OnExpanded;
  FTaskDialog.OnExpanded := TaskDialogExpanded;

  if FTaskDialog.Caption <> '' then
    Caption := FTaskDialog.Caption
  else
    Caption := ExtractFileName(Application.ExeName);

  HelpContext := FTaskDialog.HelpContext;
  CommonButtons := FTaskDialog.CommonButtons;
  DefaultButton := FTaskDialog.DefaultButton;
  if FTaskDialog is TStyledTaskDialog then
  begin
    LStyledDialog := TStyledTaskDialog(FTaskDialog);
    //Hide Close button if required
    if LStyledDialog.HideSystemCloseButton then
      BorderIcons := [];
    FUseMessageDefaultButton := LStyledDialog.UseMessageDefaultButton;
    FMessageDefaultButton := LStyledDialog.MessageDefaultButton;
    ButtonsWidth := LStyledDialog.ButtonsWidth;
    ButtonsHeight := LStyledDialog.ButtonsHeight;
  end;
  Buttons := FTaskDialog.Buttons;
  RadioButtons := FTaskDialog.RadioButtons;
  AddCustomButtons(FTaskDialog.Buttons);

  UpdateButtonsVisibility;
  UpdateButtonsSize;

  if FDialogBtnFamily <> '' then
    InitDlgButtonsWithFamily(FDialogBtnFamily);

  MainIcon := FTaskDialog.MainIcon;
  CustomMainIcon := FTaskDialog.CustomMainIcon;
  Flags := FTaskDialog.Flags;
  ExpandButtonCaption := FTaskDialog.ExpandButtonCaption;
  ExpandedText := FTaskDialog.ExpandedText;
  CustomFooterIcon := FTaskDialog.CustomFooterIcon;
  FooterIcon := FTaskDialog.FooterIcon;
  FooterText := FTaskDialog.FooterText;
  FRadioButton := FTaskDialog.RadioButton;
  ProgressBar := FTaskDialog.ProgressBar;
(*
    property URL: string read FURL;
*)
  TextMessage := FTaskDialog.Text;
  TitleMessage :=  FTaskDialog.Title;
  VerificationText := FTaskDialog.VerificationText;

  if FTaskDialog is TStyledTaskDialog then
    TStyledTaskDialog(FTaskDialog).OnFindDialogButton := FindButton;

  //Load and show Image
  LoadDialogImage;
end;

procedure TStyledTaskDialogForm.TaskDialogExpanded(Sender: TObject);
begin
  CalcMessageText(Expanded);
  AdjustHeight;
  AdjustControlsTopPos;
  if Assigned(FTaskDialogExpanded) then
    FTaskDialogExpanded(Sender);
end;

function TStyledTaskDialogForm.TaskDialogIconToImageIndex(
  const ATaskDialogIcon: TTaskDialogIcon): Integer;
begin
  Result := ATaskDialogIcon;
end;

function TStyledTaskDialogForm.TaskDialogIconToImageName(
  const ATaskDialogIcon: TTaskDialogIcon): string;
begin
  case ATaskDialogIcon of
    tdiWarning: Result := 'Warning';
    tdiError: Result := 'Error';
    tdiInformation: Result := 'Information';
    tdiShield: Result := 'Shield';
    tdiQuestion: Result := 'Question';
    tdiNone: Result := 'Custom';
  else
    Result := 'Custom';
  end;
end;

procedure TStyledTaskDialogForm.TextLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  if (LinkType = sltURL) and Assigned(FTaskDialog.OnHyperlinkClicked) then
  begin
    if (FTaskDialog is TStyledTaskDialog) then
      TStyledTaskDialog(FTaskDialog).DoOnHyperlinkClicked(Link)
  end
  else
    ShellExecute(Self.Handle, 'open' , PChar(Link), nil, nil, SW_SHOW );
end;

procedure TStyledTaskDialogForm.TimerEvent(Sender: TObject);
var
  LReset: Boolean;
begin
  Inc(FTickCount, FTimer.Interval);
  LReset := False;
  FOnTimer(Sender, FTickCount, LReset);
  if LReset then
    FTickCount := 0;
end;

procedure TStyledTaskDialogForm.VerificationCheckBoxClick(Sender: TObject);
begin
  if VerificationCheckBox.Checked then
    FTaskDialog.Flags := FTaskDialog.Flags + [tfVerificationFlagChecked]
  else
    FTaskDialog.Flags := FTaskDialog.Flags - [tfVerificationFlagChecked];
  if Assigned(FTaskDialog.OnVerificationClicked) then
    FTaskDialog.OnVerificationClicked(FTaskDialog);
end;

procedure TStyledTaskDialogForm.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = TDM_SET_PROGRESS_BAR_POS then
  begin
    InternalProgressBar.Position := Message.WParam;
  end;
end;

procedure TStyledTaskDialogForm.ButtonClick(Sender: TObject);
var
  LModalResult: TModalResult;
begin
  LModalResult := (Sender as TStyledButton).ModalResult;
  Close;
  Self.ModalResult := LModalResult;
end;

constructor TStyledTaskDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  FCommonButtons := [tcbOk, tcbCancel];
  FDefaultButton := tcbOk;
  FDialogBtnFamily := DEFAULT_CLASSIC_FAMILY;
end;

procedure TStyledTaskDialogForm.DefaultDialogSize(out AClientWidth, AClientHeight, AImageSize: Integer);
var
  LScaleFactor: Single;
begin
  LScaleFactor := GetScaleFactor;
  //Values for 96 DPI
  AClientWidth := Round(DEFAULT_STYLEDDIALOG_MIN_WIDTH * LScaleFactor);
  AClientHeight := Round(DEFAULT_STYLEDDIALOG_MIN_HEIGHT * LScaleFactor);
  AImageSize := Round(DEFAULT_MAIN_ICON_SIZE * LScaleFactor);
end;

destructor TStyledTaskDialogForm.Destroy;
begin
  inherited;
end;

procedure TStyledTaskDialogForm.ExpandButtonClick(Sender: TObject);
begin
  if FTaskDialog is TStyledTaskDialog then
    TStyledTaskDialog(FTaskDialog).DoOnExpandButtonClicked(Expanded);
end;

procedure TStyledTaskDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  LCanClose: Boolean;
begin
  LCanClose := True;
  if Assigned(FTaskDialog.OnButtonClicked) then
    FTaskDialog.OnButtonClicked(Sender, ModalResult, LCanClose);
  if not LCanClose then
    Action := TCloseAction.caNone;
end;

procedure TStyledTaskDialogForm.FormCreate(Sender: TObject);
begin
  TextLabel.Caption := '';
end;

procedure TStyledTaskDialogForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FCustomIcons[mtWarning].Free;
  FCustomIcons[mtError].Free;
  FCustomIcons[mtInformation].Free;
  FCustomIcons[mtConfirmation].Free;
  FCustomIcons[mtCustom].Free;
  FTaskDialog.OnExpanded := FTaskDialogExpanded;
  FTimer.Free;
end;

function TStyledTaskDialogForm.GetFocusedButton: TStyledButton;
var
  I: Integer;
begin
  for I := 0 to ComponentCount -1 do
  begin
    if (Components[I] is TStyledButton) then
    begin
      Result := TStyledButton(Components[I]);
      if Result.Focused then
        Exit;
    end;
  end;
  Result := FFocusedButton;
end;

procedure TStyledTaskDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  LButton: TStyledButton;
begin
  if Key = VK_ESCAPE then
    CancelButton.Click
  else if Key = VK_RETURN then
  begin
    LButton := GetFocusedButton;
    if (LButton <> nil) and (LButton.Enabled) then
      LButton.Click
    else if OKButton.Enabled then
      OKButton.Click;
  end;
end;

procedure TStyledTaskDialogForm.PlayMessageDlgSound;
const
  Sounds: array [TMsgDlgType] of integer = (
    MB_ICONEXCLAMATION, MB_ICONHAND, MB_OK,
    MB_ICONQUESTION, MB_ICONASTERISK);
begin
  MessageBeep(Sounds[FDialogType]);
end;

procedure TStyledTaskDialogForm.RadioButtonClick(Sender: TObject);
var
  LRadioButton: TRadioButton;
begin
  if FTaskDialog is TStyledTaskDialog then
  begin
    LRadioButton := Sender as TRadioButton;
    TStyledTaskDialog(FTaskDialog).DoOnRadioButtonClicked(LRadioButton.Tag);
  end;
end;

procedure TStyledTaskDialogForm.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if FTaskDialog is TStyledTaskDialog then
      TStyledTaskDialog(FTaskDialog).Handle := Self.Handle;

    AdjustButtonsCaption;
    ShowDialogForm;
    AdjustWidth;
    AutoSizeLabel.AutoSize := True;
    //Enlarge form when the message is very long
    if AutoSizeLabel.Height > Self.Monitor.Height then
    begin
      AutoSizeLabel.AutoSize := False;
      Width := Round(Self.Monitor.Height - 100 * GetScaleFactor);
      AutoSizeLabel.AutoSize := True;
    end;

    //Expand panel if Flag is present
    if (tfExpandedByDefault in FTaskDialog.Flags) then
      ExpandButton.Click;

    AdjustHeight;
    AdjustControlsTopPos;
    PlayMessageDlgSound;
    FocusDefaultButton;
    VerificationCheckBox.Checked := tfVerificationFlagChecked in TaskDialog.Flags;
    //Call event handler OnDialogCreated
    if Assigned(FTaskDialog.OnDialogCreated) then
      FTaskDialog.OnDialogCreated(FTaskDialog);
    if (FTaskDialog is TStyledTaskDialog) and
      Assigned(TStyledTaskDialog(FTaskDialog).OnDialogShow) then
      TStyledTaskDialog(FTaskDialog).OnDialogShow(Self);

    //Start Timer for ProgressBar
    if ProgressBarPanel.Visible and (tfCallbackTimer in FFlags) then
    begin
      FTimer := TTimer.Create(Self);
      FTimer.Interval := 200;
      FTimer.OnTimer := TimerEvent;
      FTickCount := 0;
      FTimer.Enabled := True;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TStyledTaskDialogForm.GetFooterText: string;
begin
  Result := FooterTextLabel.Caption;
end;

function TStyledTaskDialogForm.GetHelpContext: Integer;
begin
  Result := inherited HelpContext;
end;

procedure TStyledTaskDialogForm.GetIconNameAndIndex(
  ATaskDialogIcon: TTaskDialogIcon; out AImageName: string; out AImageIndex: Integer);
begin
  AImageIndex := TaskDialogIconToImageIndex(ATaskDialogIcon);
  AImageName := TaskDialogIconToImageName(ATaskDialogIcon);
end;

function TStyledTaskDialogForm.GetMainIconSize: Integer;
begin
  Result := ImagePanel.Width;
end;

function TStyledTaskDialogForm.GetScaleFactor: Single;
begin
  Result :={$IFDEF D10_3+}Self.ScaleFactor{$ELSE}1{$ENDIF};
end;

function TStyledTaskDialogForm.GetText: string;
begin
  Result := FText;
end;

function TStyledTaskDialogForm.GetTitle: string;
begin
  Result :=  TitleLabel.Caption;
end;

function TStyledTaskDialogForm.GetVerificationText: string;
begin
  Result := VerificationCheckBox.Caption;
end;

procedure TStyledTaskDialogForm.HelpButtonClick(Sender: TObject);
begin
  ModalResult := mrNone;
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
  AbortButton.Caption := STR_ABORT;
  IgnoreButton.Caption := STR_IGNORE;
  AllButton.Caption := STR_ALL;
  NoToAllButton.Caption := STR_NOTOALL;
  YesToAllButton.Caption := STR_YESTOALL;
  CloseButton.Caption := STR_CLOSE;
end;

procedure TStyledTaskDialogForm.Loaded;
begin
  Position := _DialogPosition;
  TextLabel.Align := alTop;
  inherited;
end;

function TStyledTaskDialogForm.ModalResultToCommonButton(
  const AModalResult: TModalResult;
  out ACommonButton: TTaskDialogCommonButton): Boolean;
begin
  case AModalResult of
    mrOK: ACommonButton := tcbOk;
    mrYes: ACommonButton := tcbYes;
    mrNo: ACommonButton := tcbNo;
    mrCancel: ACommonButton := tcbCancel;
    mrRetry: ACommonButton := tcbRetry;
    mrClose: ACommonButton := tcbClose;
  else
    ACommonButton := tcbOk;
  end;
  Result := ACommonButton in CommonButtons;
end;

procedure TStyledTaskDialogForm.InitDlgButtonsWithFamily(const AFamily: TStyledButtonFamily);

  procedure UpdateButtonStyle(const AButton: TStyledButton);
  begin
    if AButton.Visible then
      AButton.SetButtonStyle(AFamily, AButton.ModalResult);
  end;

begin
  UpdateButtonStyle(YesButton);
  UpdateButtonStyle(NoButton);
  UpdateButtonStyle(OKButton);
  UpdateButtonStyle(CancelButton);
  UpdateButtonStyle(AbortButton);
  UpdateButtonStyle(RetryButton);
  UpdateButtonStyle(IgnoreButton);
  UpdateButtonStyle(AllButton);
  UpdateButtonStyle(NoToAllButton);
  UpdateButtonStyle(YesToAllButton);
  UpdateButtonStyle(HelpButton);
  UpdateButtonStyle(CloseButton);
end;

procedure TStyledTaskDialogForm.SetPosition(const X, Y: Integer);
var
  Rect: TRect;
  LX, LY: Integer;
  LHandle: HMONITOR;
  LMonitorInfo: TMonitorInfo;
begin
  LX := X;
  LY := Y;
  Assert(Owner is TForm);
  LHandle := MonitorFromWindow(TForm(Owner).Handle, MONITOR_DEFAULTTONEAREST);
  LMonitorInfo.cbSize := SizeOf(LMonitorInfo);
  if GetMonitorInfo(LHandle, {$IFNDEF CLR}@{$ENDIF}LMonitorInfo) then
    with LMonitorInfo do
    begin
      GetWindowRect(Self.Handle, Rect);
      if LX < 0 then
        LX := ((rcWork.Right - rcWork.Left) - (Rect.Right - Rect.Left)) div 2;
      if LY < 0 then
        LY := ((rcWork.Bottom - rcWork.Top) - (Rect.Bottom - Rect.Top)) div 2;
      Inc(LX, rcWork.Left);
      Inc(LY, rcWork.Top);
      SetWindowPos(Handle, 0, LX, LY, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
    end;
end;

{ TTaskDialogLauncherHandler }

function TTaskDialogLauncherHandler.DoExecute(ParentWnd: HWND;
  const ADialogType: TMsgDlgType;
  const ATaskDialog: TStyledTaskDialog;
  const ADialogBtnFamily: TStyledButtonFamily): Boolean;
var
  LForm: TStyledTaskDialogForm;
  LParentControl: TControl;
  LOwnerForm: TForm;
  LFont: TFont;
  LDlgBtnFamily: TStyledButtonFamily;
begin
  //At least OK Button
  if (ATaskDialog.Buttons.Count = 0) and (ATaskDialog.CommonButtons = []) then
    ATaskDialog.CommonButtons := [TTaskDialogCommonButton.tcbOk];

  if tfPositionRelativeToWindow in ATaskDialog.Flags then
    _DialogPosition := poOwnerFormCenter
  else
    _DialogPosition := poScreenCenter;

  LOwnerForm := nil;
  if ParentWnd <> 0 then
  begin
    LParentControl := FindControl(ParentWnd);
    while Assigned(LParentControl) do
    begin
      if LParentControl is TForm then
        LOwnerForm := TForm(LParentControl);
      LParentControl := LParentControl.Parent;
    end;
  end;

  if ATaskDialog.UseAnimations then
  begin
    if not Assigned(_AnimatedTaskDialogFormClass) then
      raise EStyledTaskDialogException.CreateFmt(
        ERR_DIALOG_FORM_NOT_REGISTERED,
        ['Skia.Vcl.StyledTaskDialogAnimatedUnit.pas'])
    else
      LForm := _AnimatedTaskDialogFormClass.Create(LOwnerForm)
  end
  else
    LForm := _TaskDialogFormClass.Create(LOwnerForm);
  try
    //Call event handler OnDialogConstructed
    if Assigned(ATaskDialog.OnDialogConstructed) then
      ATaskDialog.OnDialogConstructed(ATaskDialog);

    if ATaskDialog.IsCustomPosition then
    begin
      LForm.Position := poDefault;
      LForm.SetPosition(ATaskDialog.Position.X, ATaskDialog.Position.Y);
    end;

    //Assign called TaskDialog component
    LForm.FTaskDialog := ATaskDialog;

    //Assign all events of TaskDialog
    LForm.OnTimer := ATaskDialog.OnTimer;

    //Assign Size of Main Icon
    LForm.MainIconSize := ATaskDialog.MainIconSize;

    LForm.FDialogType := ADialogType;
    LForm.FDialogBtnFamily := ADialogBtnFamily;
    LForm.FButtonsWidth := ATaskDialog.ButtonsWidth;
    LForm.FButtonsHeight := ATaskDialog.ButtonsHeight;
    LForm.FAutoClickDelay := ATaskDialog.AutoClickDelay;
    LForm.FAutoClick := ATaskDialog.AutoClick;
    LFont := GetDialogFont;
    LForm.AlphaBlendValue := ATaskDialog.AlphaBlendValue;
    LForm.AlphaBlend := LForm.AlphaBlendValue <> DEFAULT_STYLEDDIALOG_ALPHABLEND;
    LDlgBtnFamily := GetDialogBtnFamily;
    if Assigned(LFont) then
      LForm.SetDialogFont(LFont)
    else
      LForm.SetDialogFont(Screen.MessageFont);

    //Show the Dialog in Modal mode
    LForm.ShowModal;

    ATaskDialog.ModalResult := LForm.ModalResult;
    ATaskDialog.Button := TTaskDialogButtonItem(ATaskDialog.Buttons.FindButton(ATaskDialog.ModalResult));
    //ATaskDialog.RadioButton := TTaskDialogRadioButtonItem(ATaskDialog.RadioButtons.FindButton(LRadioButton));
    Result := True;
  finally
    LForm.Free;
    if Assigned(ATaskDialog.OnDialogDestroyed) then
      ATaskDialog.OnDialogDestroyed(ATaskDialog);
  end;
end;

initialization
  _DialogPosition := poScreenCenter;
  //Init default Dialog buttons Styles
  SetLength(_DlgButtonClasses, Ord(TMsgDlgBtn.mbClose)+1);

end.
