{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is UnitVersioningTestDLL.dpr.                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) Uwe Schuster. All rights reserved.            }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ sample for TUnitVersioning                                                                       }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date: 2006-05-18 18:04:47 +0200 (jeu., 18 mai 2006) $

library UnitVersioningTestDLL;

{$I jcl.inc}

uses
  JclUnitVersioning;

const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision: 1658 $';
    Date: '$Date: 2006-05-18 18:04:47 +0200 (jeu., 18 mai 2006) $';
    LogPath: '';
  );

begin
  RegisterUnitVersion(HInstance, UnitVersioning);
end.
