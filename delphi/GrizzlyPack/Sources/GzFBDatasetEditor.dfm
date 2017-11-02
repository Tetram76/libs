object DlgFBDatasetEditor: TDlgFBDatasetEditor
  Left = 327
  Top = 204
  Width = 749
  Height = 460
  Caption = 'GzFBDataSet Editor'
  Color = clBtnFace
  Constraints.MaxWidth = 749
  Constraints.MinHeight = 383
  Constraints.MinWidth = 749
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    741
    433)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TBitBtn
    Left = 556
    Top = 400
    Width = 89
    Height = 29
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BitBtn1: TBitBtn
    Left = 648
    Top = 400
    Width = 89
    Height = 29
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Kind = bkCancel
  end
  object PageControl1: TPageControl
    Left = 4
    Top = 4
    Width = 733
    Height = 393
    ActivePage = tsGenerer
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 2
    object tsGenerer: TTabSheet
      Caption = 'G'#233'n'#233'rer'
      ImageIndex = 1
      OnShow = tsGenererShow
      DesignSize = (
        725
        365)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 79
        Height = 13
        Caption = 'Table '#224' '#233'diter'
      end
      object LabelErreurs: TLabel
        Left = 8
        Top = 96
        Width = 145
        Height = 41
        AutoSize = False
        Caption = 'Erreurs'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold, fsItalic]
        ParentFont = False
        WordWrap = True
      end
      object Label2: TLabel
        Left = 156
        Top = 8
        Width = 135
        Height = 13
        Caption = 'Cl'#233' pour Update/Delete'
      end
      object Label3: TLabel
        Left = 344
        Top = 8
        Width = 97
        Height = 13
        Caption = 'Cl'#233' pour Refresh'
      end
      object Label4: TLabel
        Left = 532
        Top = 8
        Width = 138
        Height = 13
        Caption = 'Champs '#224' mettre '#224' jour'
      end
      object btnGenerer: TBitBtn
        Left = 8
        Top = 68
        Width = 145
        Height = 25
        Caption = 'G'#233'n'#233'rer le SQL'
        TabOrder = 0
        OnClick = btnGenererClick
      end
      object LBUpdateKey: TListBox
        Left = 156
        Top = 24
        Width = 185
        Height = 309
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
      end
      object CBTable: TComboBox
        Left = 8
        Top = 24
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        Sorted = True
        TabOrder = 2
        OnChange = CBTableChange
      end
      object LBRefreshKey: TListBox
        Left = 344
        Top = 24
        Width = 185
        Height = 309
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 3
      end
      object LBUpdateFields: TListBox
        Left = 532
        Top = 24
        Width = 185
        Height = 333
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 4
      end
      object CBGuillemets: TCheckBox
        Left = 8
        Top = 48
        Width = 97
        Height = 17
        Caption = 'Guillemets'
        TabOrder = 5
      end
      object CBIndexUpdate: TComboBox
        Left = 156
        Top = 336
        Width = 185
        Height = 22
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ItemHeight = 14
        ParentFont = False
        TabOrder = 6
        OnChange = CBIndexUpdateChange
      end
      object CBIndexRefresh: TComboBox
        Left = 344
        Top = 336
        Width = 185
        Height = 22
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ItemHeight = 14
        ParentFont = False
        TabOrder = 7
        OnChange = CBIndexRefreshChange
      end
    end
    object tsSQL: TTabSheet
      Caption = 'SQL'
      OnShow = tbSQLChange
      DesignSize = (
        725
        365)
      object tbSQL: TTabControl
        Left = 2
        Top = 2
        Width = 721
        Height = 361
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 1
        Tabs.Strings = (
          'Select'
          'Update'
          'Insert'
          'Delete'
          'Refresh')
        TabIndex = 0
        OnChange = tbSQLChange
      end
      object MemoSQL: TSynMemo
        Left = 6
        Top = 26
        Width = 713
        Height = 332
        Anchors = [akLeft, akTop, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Highlighter = SynSQLSyn1
        Lines.Strings = (
          'MemoSQL')
        WantTabs = True
        OnChange = MemoSQLChange
      end
    end
  end
  object SynSQLSyn1: TSynSQLSyn
    TableNameAttri.Foreground = clOlive
    TableNameAttri.Style = [fsUnderline]
    SQLDialect = sqlInterbase6
    Left = 348
    Top = 80
  end
  object transReadOnly: TUIBTransaction
    Options = [tpConcurrency, tpNowait]
    Left = 52
    Top = 172
  end
  object dtsIndices: TUIBQuery
    SQL.Strings = (
      
        'select RDB$INDICES.RDB$INDEX_ID, RDB$INDEX_SEGMENTS.RDB$FIELD_NA' +
        'ME '
      
        'from RDB$INDICES inner join RDB$INDEX_SEGMENTS on RDB$INDICES.RD' +
        'B$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME '
      'where '
      'RDB$INDICES.RDB$UNIQUE_FLAG = 1 and'
      'RDB$INDICES.RDB$RELATION_NAME = :TABLE_NAME '
      'order by'
      'RDB$INDICES.RDB$INDEX_ID,'
      'RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION')
    Transaction = transReadOnly
    Left = 52
    Top = 204
  end
  object dtsCheckSQL: TUIBDataSet
    Transaction = transReadOnly
    SQL.Strings = (
      'select '
      'CLIENTS.SOC_KEYID, '
      'CLI_KEYID, '
      'CLI_NOMINTERNE, '
      'CLI_NOMSOCIETE, '
      'CLI_CODEPOSTAL, '
      'CLI_VILLE,'
      'CLI_COMMENTAIRES,'
      'CLI_ACTIF,'
      'CCL_CATEGORIE'
      'from '
      
        'CLIENTS left join CATEGORIESCLIENTS on CLIENTS.CCL_KEYID=CATEGOR' +
        'IESCLIENTS.CCL_KEYID'
      'where '
      'CLI_ACTIF = :CLI_ACTIF'
      'order by CLI_KEYID'
      '')
    Left = 84
    Top = 204
  end
  object dtsGetFields: TUIBDataSet
    Transaction = transReadOnly
    Left = 120
    Top = 204
  end
end
