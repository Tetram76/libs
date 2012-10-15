{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditHighlighterOptions.pas, released 2012-09-12.

All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id: SynEditHighlighterOptions.pas,v 1.0 2012/09/12 08:31:23 codehunterworks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditHighlighterOptions;

interface

uses
  Classes;

type
  TSynEditHighlighterOptions = class(TPersistent)
  private
    FAutoDetectEnabled: Boolean;
    FAutoDetectMatchExpression: String;
    FAutoDetectLineLimit: Cardinal;
    FVisible: Boolean;
    FLineCommentarStart: String;
    FLineCommentarEnd: String;
    FTitle: String;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property AutoDetectEnabled: Boolean read FAutoDetectEnabled write FAutoDetectEnabled;
    property AutoDetectLineLimit: Cardinal read FAutoDetectLineLimit write FAutoDetectLineLimit;
    property AutoDetectMatchExpression: String read FAutoDetectMatchExpression write FAutoDetectMatchExpression;
    property LineCommentarEnd: String read FLineCommentarEnd write FLineCommentarEnd;
    property LineCommentarStart: String read FLineCommentarStart write FLineCommentarStart;
    property Title: String read FTitle write FTitle;
    property Visible: Boolean read FVisible write FVisible;
  end;

implementation

procedure TSynEditHighlighterOptions.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TSynEditHighlighterOptions) then begin
    with TSynEditHighlighterOptions(Source) do begin
      FAutoDetectEnabled:= AutoDetectEnabled;
      FAutoDetectMatchExpression:= AutoDetectMatchExpression;
      FAutoDetectLineLimit:= AutoDetectLineLimit;
      FLineCommentarStart:= LineCommentarStart;
      FLineCommentarEnd:= LineCommentarEnd;
      FVisible:= Visible;
    end;
  end;
end;

procedure TSynEditHighlighterOptions.AssignTo(Dest: TPersistent);
begin
  if Dest.InheritsFrom(TSynEditHighlighterOptions) then begin
    with TSynEditHighlighterOptions(Dest) do begin
      AutoDetectEnabled:= FAutoDetectEnabled;
      AutoDetectMatchExpression:= FAutoDetectMatchExpression;
      AutoDetectLineLimit:= FAutoDetectLineLimit;
      LineCommentarStart:= FLineCommentarStart;
      LineCommentarEnd:= FLineCommentarEnd;
      Visible:= FVisible;
    end;
  end;
end;

end.
