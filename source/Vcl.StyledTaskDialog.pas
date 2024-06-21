{******************************************************************************}
{                                                                              }
{  StyledTaskDialog: a Task Dialog Component with StyleButtons                 }
{                                                                              }
{  Copyright (c) 2022-2024 (Ethea S.r.l.)                                      }
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
unit Vcl.StyledTaskDialog;

{$INCLUDE StyledComponents.inc}

interface

uses
  System.SysUtils
  , System.Classes
  , WinApi.Windows
  , Vcl.Dialogs
  , Vcl.Graphics
  , Vcl.Forms
  , Vcl.ButtonStylesAttributes
  ;

const
  DEFAULT_ALPHABLEND = 255;
  DEFAULT_STYLEDDIALOG_MIN_WIDTH = 500;
  DEFAULT_STYLEDDIALOG_MIN_HEIGHT = 280;

  {$IFDEF Use_Large_Dialog_Icons}
  DEFAULT_MAIN_ICON_SIZE = 128;
  {$ELSE}
  DEFAULT_MAIN_ICON_SIZE = 64;
  {$ENDIF}

type
  TStyledDialogIcons = array[TMsgDlgType] of TIcon;

{$WARN SYMBOL_PLATFORM OFF}
{ TaskDialog based message dialog; requires Windows Vista or later }
type
  { TStyledTaskDialog }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledTaskDialog = class(TTaskDialog)
  private
    FHelpFile: string;
    FParentWnd: HWND;
    FPosition: TPoint;
    FMainIconSize: Integer;
    FDialogButtonsFamily: TStyledButtonFamily;
    FUseCommandLinks: Boolean;
    FUseTitleInMessageDlg: Boolean;
    FAlphaBlendValue: Integer;
    function IsDefaultFamily: Boolean;
  strict protected
    function DoExecute(ParentWnd: HWND): Boolean; override;
    procedure DoOnDialogCreated; override;
    procedure DoOnHelp; override;
  public
    procedure DoOnRadioButtonClicked(ButtonID: Integer); override;
    procedure DoOnHyperlinkClicked(const AURL: string); override;
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Position: TPoint read FPosition write FPosition;
    property MainIconSize: Integer read FMainIconSize write FMainIconSize default DEFAULT_MAIN_ICON_SIZE;
  published
    property DialogButtonsFamily: TStyledButtonFamily read FDialogButtonsFamily write FDialogButtonsFamily stored IsDefaultFamily;
    property UseCommandLinks: Boolean read FUseCommandLinks write FUseCommandLinks default False;
    property UseTitleInMessageDlg: Boolean read FUseTitleInMessageDlg write FUseTitleInMessageDlg default True;
    property AlphaBlendValue: Integer read FAlphaBlendValue write FAlphaBlendValue default DEFAULT_ALPHABLEND;
  end;

  //Abstraction of a Dialog Launcher
  ITaskDialogLauncher = interface
    ['{B2F16F98-C163-4706-A803-E624126D8DF6}']
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TStyledTaskDialog;
      const ADialogBtnFamily: TStyledButtonFamily): boolean;
  end;


//Deprecated functions
function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload; deprecated 'use StyledTaskMessageDlg(...) instead'
function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) instead'
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) or StyledTaskMessageDlg(...) instead'
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) or StyledTaskMessageDlg(...) instead'
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload; deprecated 'use StyledMessageDlgPos(...) changing position of DefaultButton param, after HelpCtx'

//Equivalent functions of standard MessageDlg functions
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  CustomButtonCaptions: array of string): Integer; overload;

//Equivalent functions of standard MessageDlgPos functions
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer; overload;

//Equivalent functions of standard TaskMessageDlg functions
function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
//Equivalent functions of standard TaskMessageDlgPos functions
function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer; overload;

//Equivalent function of standard ShowMessage function
procedure StyledShowMessage(const Msg: string); overload;

procedure SetUseTitleInMessageDlg(AValue: boolean);
procedure RegisterCustomExecute(const AShowStyledTaskDialog: ITaskDialogLauncher;
  const AButtonFamily: TStyledButtonFamily = '');
procedure UnregisterCustomExecute;
procedure InitializeStyledTaskDialogs(
  AUseTitleInMessageDlg: Boolean;
  AFont: TFont; AUseCommandLinks: Boolean = False;
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AAlphaBlendValue: Byte = DEFAULT_ALPHABLEND);
function GetTaskDlgType(const AIcon: TTaskDialogIcon): TMsgDlgType;
function GetDialogFont: TFont;
function GetDialogBtnFamily: TStyledButtonFamily;

function GetDialogTypeTitle(const DlgType: TMsgDlgType): string;
function GetDialogAlphaBlendValue: Byte;

implementation

uses
  System.TypInfo
  , System.Math
  , System.Types
  , Vcl.Themes
  , Winapi.CommCtrl
  , System.WideStrUtils
  , Winapi.MultiMon
  , System.HelpIntfs
  , System.UITypes
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Consts
  , Winapi.ShellApi
  , Vcl.StyledCmpMessages
  , Vcl.StyledButton
  , Vcl.StandardButtonStyles
  , Vcl.StyledCmpStrUtils
  , Vcl.StyledTaskDialogStdUnit
  ;

var
  _TaskDialogExecute: ITaskDialogLauncher;
  _DialogButtonsFamily: TStyledButtonFamily;
  _CustomIcons: TStyledDialogIcons;
  _DialogFont: TFont;
  _UseCommandLinks: Boolean;
  _UseTitleInMessageDlg: boolean;
  _AlphaBlendValue: Byte;

  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll','Help','Close');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);

function GetDefaultButton(const Buttons: TMsgDlgButtons): TMsgDlgBtn;
begin
  if      mbYes       in Buttons then Result := TMsgDlgBtn.mbYes
  else if mbOK        in Buttons then Result := TMsgDlgBtn.mbOK
  else if mbNo        in Buttons then Result := TMsgDlgBtn.mbNo
  else if mbCancel    in Buttons then Result := TMsgDlgBtn.mbCancel
  else if mbAbort     in Buttons then Result := TMsgDlgBtn.mbAbort
  else if mbRetry     in Buttons then Result := TMsgDlgBtn.mbRetry
  else if mbIgnore    in Buttons then Result := TMsgDlgBtn.mbIgnore
  else if mbAll       in Buttons then Result := TMsgDlgBtn.mbAll
  else if mbNoToAll   in Buttons then Result := TMsgDlgBtn.mbNoToAll
  else if mbYesToAll  in Buttons then Result := TMsgDlgBtn.mbYesToAll
  else if mbHelp      in Buttons then Result := TMsgDlgBtn.mbHelp
  else if mbClose     in Buttons then Result := TMsgDlgBtn.mbClose
  else Result := TMsgDlgBtn.mbRetry;
end;

//Internal Function to Display a Styled TaskMessage Dialog
//using a TStyledTaskDialog component
function InternalDoTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X, Y: Integer;
  const HelpFileName: string;
  DefaultButton: TMsgDlgBtn;
  UseAlternateTaskDlgFlags: Boolean;
  AlternateTaskDlgFlags: TTaskDialogFlags;
  CustomButtonCaptions: array of string): Integer;
const
  IconMap: array[TMsgDlgType] of TTaskDialogIcon = (tdiWarning, tdiError,
    tdiInformation, tdiInformation, tdiNone);
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp, mrClose);
var
  DlgBtn: TMsgDlgBtn;
  LTaskDialog: TStyledTaskDialog;
  LTaskDialogButtonItem: TTaskDialogButtonItem;
  LUseCommandLinks: Boolean;
  LIndex: Integer;
begin
  //At least OK Button
  if Buttons = [] then
    Buttons := [TMsgDlgBtn.mbOK];

  Application.ModalStarted;
  LTaskDialog := TStyledTaskDialog.Create(nil);
  try
    if UseAlternateTaskDlgFlags then
      LTaskDialog.Flags := AlternateTaskDlgFlags; // replace  the default Flags to AlternateTaskDlgFlags.
    // Assign buttons
    LIndex := -1;
    LUseCommandLinks := True;
    for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if DlgBtn in Buttons then
      begin
        LTaskDialogButtonItem := LTaskDialog.Buttons.Add as TTaskDialogButtonItem;
        Inc(LIndex);
        if LIndex <= High(CustomButtonCaptions) then
          LTaskDialogButtonItem.Caption := CustomButtonCaptions[LIndex]
        else
        begin
          //use Captions defined in Vcl.StyledCmpMessages.pas
          case DlgBtn of
            mbYes: LTaskDialogButtonItem.Caption := STR_YES;
            mbNo: LTaskDialogButtonItem.Caption := STR_NO;
            mbOK: LTaskDialogButtonItem.Caption := STR_OK;
            mbCancel: LTaskDialogButtonItem.Caption := STR_CANCEL;
            mbAbort: LTaskDialogButtonItem.Caption := STR_ABORT;
            mbRetry: LTaskDialogButtonItem.Caption := STR_RETRY;
            mbIgnore: LTaskDialogButtonItem.Caption := STR_IGNORE;
            mbAll: LTaskDialogButtonItem.Caption := STR_ALL;
            mbNoToAll: LTaskDialogButtonItem.Caption := STR_NOTOALL;
            mbYesToAll: LTaskDialogButtonItem.Caption := STR_YESTOALL;
            mbHelp: LTaskDialogButtonItem.Caption := STR_HELP;
            mbClose: LTaskDialogButtonItem.Caption := STR_CLOSE;
          end;
        end;

        //Set CommandLinkHint based on DlgBtn when using a Warning or Confirmation
        if DlgType in [TMsgDlgType.mtWarning, TMsgDlgType.mtConfirmation] then
        case DlgBtn of
          mbYes: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
          mbNo: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbCancel: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbAbort: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_ABORT, STR_THE_OPERATION]);
          mbRetry: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_RETRY, STR_THE_OPERATION]);
          mbIgnore: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_IGNORE, STR_THE_OPERATION]);
          mbNoToAll: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbYesToAll: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
        end;
        LUseCommandLinks := LUseCommandLinks and (LTaskDialogButtonItem.CommandLinkHint <> '');

        if DlgBtn = DefaultButton then
          LTaskDialogButtonItem.Default := True;
        LTaskDialogButtonItem.ModalResult := LModalResults[DlgBtn];
      end;
    end;

    //use Captions defined in Vcl.StyledCmpMessages.pas
    LTaskDialog.Caption := GetDialogTypeTitle(DlgType);
    LTaskDialog.CommonButtons := [];
    if Not UseAlternateTaskDlgFlags and Application.UseRightToLeftReading then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfRtlLayout];
    if pos('<A HREF=',Msg) > 0 then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfEnableHyperlinks];

    if LTaskDialog.UseCommandLinks and LUseCommandLinks then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfUseCommandLinks];

    LTaskDialog.HelpContext := HelpCtx;
    LTaskDialog.HelpFile := HelpFileName;
    LTaskDialog.MainIcon := IconMap[DlgType];
    LTaskDialog.Position := Point(X, Y);
    LTaskDialog.Text := Msg;

    if (Instruction = '') and (LTaskDialog.UseTitleInMessageDlg) then
      LTaskDialog.Title := LTaskDialog.Caption
    else
      LTaskDialog.Title := Instruction;

    // Show dialog and return result
    Result := mrNone;
    if LTaskDialog.Execute then
      Result := LTaskDialog.ModalResult;
  finally
    LTaskDialog.Free;
    Application.ModalFinished;
  end;
end;

//Begin of Internal Functions
  function DoTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1;
    const HelpFileName: string = ''): Integer; overload; inline;
  begin
    Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
      Buttons, HelpCtx, X, Y, HelpFileName, GetDefaultButton(Buttons), False, [], []);
  end;

  function DoTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
    X: Integer = -1; Y: Integer = -1; const HelpFileName: string = ''): Integer; overload; inline;
  begin
    Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
      Buttons, HelpCtx, X, Y, HelpFileName, DefaultButton, False, [], []);
  end;

  function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Integer; DefaultButton: TMsgDlgBtn;
    X: Integer = -1; Y: Integer = -1; const HelpFileName: string = ''): Integer; overload; inline;
  begin
    Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType,
      Buttons, HelpCtx, X, Y, HelpFileName, DefaultButton, False, [], []);
  end;

  function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint;
    X: Integer = -1; Y: Integer = -1; const HelpFileName: string = ''): Integer; overload; inline;
  begin
    Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType,
      Buttons, HelpCtx, X, Y, HelpFileName, GetDefaultButton(Buttons), False, [], []);
  end;
//End of Internal Functions

function GetDialogAlphaBlendValue: Byte;
begin
  Result := _AlphaBlendValue;
end;

function GetDialogFont: TFont;
begin
  Result := _DialogFont;
end;

function GetDialogBtnFamily: TStyledButtonFamily;
begin
  Result := _DialogButtonsFamily;
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := DoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx);
end;

function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := DoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx);
end;

function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
begin
  Result := DoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    DefaultButton, X, Y);
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  CustomButtonCaptions: array of string): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    -1, -1, '', DefaultButton, False, [], CustomButtonCaptions);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer): Integer;
begin
  Result := DoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx, X, Y);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer;
begin
  Result := DoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons,
    HelpCtx, DefaultButton, X, Y);
end;


procedure InitializeStyledTaskDialogs(AUseTitleInMessageDlg: Boolean;
  AFont: TFont; AUseCommandLinks: Boolean = False;
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AAlphaBlendValue: Byte = DEFAULT_ALPHABLEND);
begin
  if Assigned(AFont) then
  begin
    if not Assigned(_DialogFont) then
      _DialogFont := TFont.Create;
    _DialogFont.Assign(AFont);
  end
  else
    FreeAndNil(_DialogFont);
  _UseCommandLinks := AUseCommandLinks;
  _UseTitleInMessageDlg := AUseTitleInMessageDlg;
  _DialogButtonsFamily := ADialogButtonsFamily;
  _AlphaBlendValue := AAlphaBlendValue;
end;

procedure UnregisterCustomIcons;
begin
  _CustomIcons[mtWarning] := nil;
  _CustomIcons[mtError] := nil;
  _CustomIcons[mtInformation] := nil;
  _CustomIcons[mtConfirmation] := nil;
  _CustomIcons[mtCustom] := nil;
end;

procedure RegisterCustomIcons(const ACustomIcons: TStyledDialogIcons);
begin
  UnregisterCustomIcons;
  _CustomIcons := ACustomIcons;
end;

procedure SetUseTitleInMessageDlg(AValue: boolean);
begin
  _UseTitleInMessageDlg := AValue;
end;

procedure RegisterCustomExecute(const AShowStyledTaskDialog: ITaskDialogLauncher;
  const AButtonFamily: TStyledButtonFamily = '');
begin
  _TaskDialogExecute := AShowStyledTaskDialog;
end;

procedure UnRegisterCustomExecute;
begin
  _TaskDialogExecute := nil;
end;

function GetTaskDlgType(
  const AIcon: TTaskDialogIcon): TMsgDlgType;
begin
  if AIcon = tdiNone then
    Result := TMsgDlgType.mtCustom
  else if AIcon = tdiWarning then
    Result := TMsgDlgType.mtWarning
  else if AIcon = tdiError then
    Result := TMsgDlgType.mtError
  else if AIcon = tdiInformation then
    Result := TMsgDlgType.mtInformation
  else if AIcon = tdiShield then
    Result := TMsgDlgType.mtCustom
  else
    Result := TMsgDlgType.mtInformation;
end;

function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := DoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx);
end;

function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1): Integer;
begin
  Result := DoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y);
end;

function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer): Integer;
begin
  Result := DoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y);
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer;
begin
  Result := StyledTaskMessageDlgPos(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton, X, Y);
end;

function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer;
var
  MsgWithTitle: string;
begin
  if Title <> '' then
    MsgWithTitle := UpperCase(Title)+sLineBreak+Msg
  else
    MsgWithTitle := Msg;
    Result := StyledMessageDlgPos(MsgWithTitle, DlgType, Buttons, HelpCtx, DefaultButton, -1, -1);
end;

function StyledTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn; CustomButtonCaptions: array of string): Integer;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    X, Y, HelpFileName, DefaultButton, False, [], []);
end;

procedure StyledShowMessage(const Msg: string); overload;
begin
  StyledMessageDlg(Msg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

{ TStyledTaskDialog }
constructor TStyledTaskDialog.Create(AOwner: TComponent);
begin
  inherited;
  FDialogButtonsFamily := _DialogButtonsFamily;
  FUseCommandLinks := _UseCommandLinks;
  FUseTitleInMessageDlg := _UseTitleInMessageDlg;
  FAlphaBlendValue := _AlphaBlendValue;
  FMainIconSize := DEFAULT_MAIN_ICON_SIZE;
end;

function TStyledTaskDialog.DoExecute(ParentWnd: HWND): Boolean;
type
  TTaskDialogIcon = (tdiWarning, tdiError,
    tdiInformation, tdiShield, tdiNone);
var
  LTaskDlgType: TMsgDlgType;
begin
  LTaskDlgType := GetTaskDlgType(MainIcon);

  //Use a custom interface for StyledTaskDialog, if registered
  if Assigned(_TaskDialogExecute) then
    Result := _TaskDialogExecute.DoExecute(ParentWnd,
      LTaskDlgType, Self, Self.FDialogButtonsFamily)
  else
    Result := inherited DoExecute(ParentWnd);
end;

procedure TStyledTaskDialog.DoOnDialogCreated;
var
  Rect: TRect;
  LX, LY: Integer;
  LHandle: HMONITOR;
  LMonitorInfo: TMonitorInfo;
begin
  LX := Position.X;
  LY := Position.Y;
  LHandle := MonitorFromWindow(FParentWnd, MONITOR_DEFAULTTONEAREST);
  LMonitorInfo.cbSize := SizeOf(LMonitorInfo);
  if GetMonitorInfo(LHandle, {$IFNDEF CLR}@{$ENDIF}LMonitorInfo) then
    with LMonitorInfo do
    begin
      GetWindowRect(Handle, Rect);
      if LX < 0 then
        LX := ((rcWork.Right - rcWork.Left) - (Rect.Right - Rect.Left)) div 2;
      if LY < 0 then
        LY := ((rcWork.Bottom - rcWork.Top) - (Rect.Bottom - Rect.Top)) div 2;
      Inc(LX, rcWork.Left);
      Inc(LY, rcWork.Top);
      SetWindowPos(Handle, 0, LX, LY, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
    end;
end;

procedure TStyledTaskDialog.DoOnHelp;
var
  LHelpFile: string;
  LHelpSystem: IHelpSystem;

{$IFNDEF D12+}
  procedure ShowHelpException(E: Exception);
  var
    Msg: string;
    Flags: Integer;
  begin
    Flags := MB_OK or MB_ICONSTOP;
    if Application.UseRightToLeftReading then
      Flags := Flags or MB_RTLREADING;
    Msg := E.Message;
    if (Msg <> '') and (AnsiLastChar(Msg) > '.') then
      Msg := Msg + '.';
    MessageBox(Application.Handle, PChar(Msg), PChar(Application.Title), Flags);
  end;
{$ENDIF}

begin
  if HelpContext <> 0 then
  begin
    if FHelpFile = '' then
      LHelpFile := Application.HelpFile
    else
      LHelpFile := HelpFile;
    if System.HelpIntfs.GetHelpSystem(LHelpSystem) then
    try
      LHelpSystem.Hook(Application.Handle, LHelpFile, HELP_CONTEXT, HelpContext);
    except
      on E: Exception do
        ShowHelpException(E);
    end;
  end;
end;

procedure TStyledTaskDialog.DoOnHyperlinkClicked(const AURL: string);
begin
  inherited DoOnHyperlinkClicked(AURL);
end;

procedure TStyledTaskDialog.DoOnRadioButtonClicked(ButtonID: Integer);
begin
  inherited DoOnRadioButtonClicked(ButtonID);
end;

function TStyledTaskDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FParentWnd := ParentWnd;
  Result := inherited Execute(ParentWnd);
end;

function TStyledTaskDialog.IsDefaultFamily: Boolean;
begin
  Result := FDialogButtonsFamily <> DEFAULT_CLASSIC_FAMILY;
end;

function GetDialogTypeTitle(const DlgType: TMsgDlgType): string;
begin
  case DlgType of
    mtWarning      : Result := STR_WARNING;
    mtError        : Result := STR_ERROR;
    mtInformation  : Result := STR_INFORMATION;
    mtConfirmation : Result := STR_CONFIRM;
    mtCustom       : Result := STR_INFORMATION;
  end;
end;

initialization
  _DialogButtonsFamily := DEFAULT_CLASSIC_FAMILY;
  _UseCommandLinks := False;
  _UseTitleInMessageDlg := True;
  _AlphaBlendValue := DEFAULT_ALPHABLEND;
  _DialogFont := nil;

finalization
  if Assigned(_DialogFont) then
    _DialogFont.Free;

end.
