{******************************************************************************}
{                                                                              }
{  StyledMessagesHooks: an interposer Unit to use Styled Dialog Boxes          }
{  using Standard Delphi calls MessageDialog or ShowMessage                    }
{                                                                              }
{  Copyright (c) 2022-2026 (Ethea S.r.l.)                                      }
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
unit Vcl.StyledMessagesHooks;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.Dialogs
  ;

const
  //Sentinel meaning "no explicit default button". TMsgDlgBtn has no 'none'
  //value, so an out-of-range cast is used as a marker.
  NO_DEFAULT_BUTTON = TMsgDlgBtn(-1);

//These interposer routines intentionally SHADOW (hide) the Vcl.Dialogs ones and
//must NOT be marked 'overload'. Marking them overload merges them into the same
//candidate set as Vcl.Dialogs' identical signatures, and every call then fails
//with E2251 "Ambiguous overloaded call". Instead each routine covers all the RTL
//call shapes (2..5 arguments, including the DefaultButton form) via optional
//trailing parameters. Do NOT "helpfully" add 'overload' here.

function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;

function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons = [mbOK]; HelpCtx: Longint = 0;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;

function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;

function TaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;

procedure ShowMessage(const Msg: string);

implementation

uses
  Vcl.StyledTaskDialog
  ;

function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;
begin
  if DefaultButton = NO_DEFAULT_BUTTON then
    Result := StyledTaskMessageDlg(Title, Msg, DlgType, Buttons, HelpCtx)
  else
    Result := StyledTaskMessageDlg(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons = [mbOK]; HelpCtx: Longint = 0;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;
begin
  if DefaultButton = NO_DEFAULT_BUTTON then
    Result := StyledMessageDlg(Msg, DlgType, Buttons, HelpCtx)
  else
    Result := StyledMessageDlg(Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;
begin
  if DefaultButton = NO_DEFAULT_BUTTON then
    Result := StyledMessageDlgPos(Msg, DlgType, Buttons, HelpCtx, X, Y)
  else
    Result := StyledMessageDlgPos(Msg, DlgType, Buttons, HelpCtx, DefaultButton, X, Y);
end;

function TaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  DefaultButton: TMsgDlgBtn = NO_DEFAULT_BUTTON): Integer;
begin
  if DefaultButton = NO_DEFAULT_BUTTON then
    Result := StyledTaskMessageDlgPos(Title, Msg, DlgType, Buttons, HelpCtx, X, Y)
  else
    Result := StyledTaskMessageDlgPos(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton, X, Y);
end;

procedure ShowMessage(const Msg: string);
begin
  StyledShowMessage(Msg);
end;

end.
