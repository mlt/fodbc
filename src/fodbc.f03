! DO NOT EDIT!
! Generated from gccxml using sql.xslt
! gccxml /usr/include/sqlext.h -fxml=/tmp/sqlext.xml && xsltproc sql.xslt /tmp/sqlext.xml sql.xslt > fodbc.f03
!
! This file is a part of fodbc, Fortran 2003 ODBC bindings
! Copyright (C) 2013 Mikhail Titov
!
! fodbc is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as
! published by the Free Software Foundation, either version 3 of the
! License, or (at your option) any later version.
!
! fodbc is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with fodbc.  If not, see <http://www.gnu.org/licenses/>.

module fodbc
  use fodbc_types
  implicit none

  interface

    integer(kind=c_short) function SQLParamOptions &
      (hstmt,crow,pirow) &
      bind(C, name="SQLParamOptions")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: crow
      integer(kind=c_long),intent(out) :: pirow
    end function SQLParamOptions

    integer(kind=c_short) function SQLGetInfo &
      (ConnectionHandle,InfoType,InfoValue,BufferLength,StringLength) &
      bind(C, name="SQLGetInfo")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: InfoType
      type(c_ptr),intent(in),value :: InfoValue
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
    end function SQLGetInfo

    integer(kind=c_short) function SQLSetStmtAttrW &
      (hstmt,fAttribute,rgbValue,cbValueMax) &
      bind(C, name="SQLSetStmtAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
    end function SQLSetStmtAttrW

    integer(kind=c_short) function SQLExecDirectA &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLExecDirectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLExecDirectA

    integer(kind=c_short) function SQLNativeSql &
      (hdbc,szSqlStrIn,cbSqlStrIn,szSqlStr,cbSqlStrMax, &
      pcbSqlStr) &
      bind(C, name="SQLNativeSql")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char),dimension(*),intent(in) :: szSqlStrIn
      integer(kind=c_long),intent(in),value :: cbSqlStrIn
      character(kind=c_char),dimension(*),intent(in) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStrMax
      integer(kind=c_long),intent(out) :: pcbSqlStr
    end function SQLNativeSql

    integer(kind=c_short) function SQLGetConnectOptionW &
      (hdbc,fOption,pvParam) &
      bind(C, name="SQLGetConnectOptionW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      type(c_ptr),intent(in),value :: pvParam
    end function SQLGetConnectOptionW

    integer(kind=c_short) function SQLDataSourcesA &
      (henv,fDirection,szDSN,cbDSNMax,pcbDSN, &
      szDescription,cbDescriptionMax,pcbDescription) &
      bind(C, name="SQLDataSourcesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      character(kind=c_char),dimension(*),intent(in) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSNMax
      integer(kind=c_short),intent(out) :: pcbDSN
      character(kind=c_char),dimension(*),intent(in) :: szDescription
      integer(kind=c_short),intent(in),value :: cbDescriptionMax
      integer(kind=c_short),intent(out) :: pcbDescription
    end function SQLDataSourcesA

    integer(kind=c_long) function ODBCGetTryWaitValue &
      () &
      bind(C, name="ODBCGetTryWaitValue")
      use, intrinsic :: iso_c_binding
    end function ODBCGetTryWaitValue

    integer(kind=c_short) function SQLDataSourcesW &
      (henv,fDirection,szDSN,cbDSNMax,pcbDSN, &
      szDescription,cbDescriptionMax,pcbDescription) &
      bind(C, name="SQLDataSourcesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      integer(kind=c_short),intent(out) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSNMax
      integer(kind=c_short),intent(out) :: pcbDSN
      integer(kind=c_short),intent(out) :: szDescription
      integer(kind=c_short),intent(in),value :: cbDescriptionMax
      integer(kind=c_short),intent(out) :: pcbDescription
    end function SQLDataSourcesW

    integer(kind=c_short) function SQLCloseCursor &
      (StatementHandle) &
      bind(C, name="SQLCloseCursor")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLCloseCursor

    integer(kind=c_short) function SQLSetEnvAttr &
      (EnvironmentHandle,Attribute,Value,StringLength) &
      bind(C, name="SQLSetEnvAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: StringLength
    end function SQLSetEnvAttr

    integer(kind=c_short) function SQLFetchScroll &
      (StatementHandle,FetchOrientation,FetchOffset) &
      bind(C, name="SQLFetchScroll")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: FetchOrientation
      integer(kind=c_long),intent(in),value :: FetchOffset
    end function SQLFetchScroll

    integer(kind=c_short) function SQLFreeEnv &
      (EnvironmentHandle) &
      bind(C, name="SQLFreeEnv")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
    end function SQLFreeEnv

    integer(kind=c_short) function SQLGetFunctions &
      (ConnectionHandle,FunctionId,Supported) &
      bind(C, name="SQLGetFunctions")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: FunctionId
      integer(kind=c_short),intent(out) :: Supported
    end function SQLGetFunctions

    integer(kind=c_short) function SQLSetParam &
      (StatementHandle,ParameterNumber,ValueType,ParameterType,LengthPrecision, &
      ParameterScale,ParameterValue,StrLen_or_Ind) &
      bind(C, name="SQLSetParam")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ParameterNumber
      integer(kind=c_short),intent(in),value :: ValueType
      integer(kind=c_short),intent(in),value :: ParameterType
      integer(kind=c_long),intent(in),value :: LengthPrecision
      integer(kind=c_short),intent(in),value :: ParameterScale
      type(c_ptr),intent(in),value :: ParameterValue
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLSetParam

    integer(kind=c_short) function SQLEndTran &
      (HandleType,Handle,CompletionType) &
      bind(C, name="SQLEndTran")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
      integer(kind=c_short),intent(in),value :: CompletionType
    end function SQLEndTran

    integer(kind=c_short) function SQLColumns &
      (StatementHandle,CatalogName,NameLength1,SchemaName,NameLength2, &
      TableName,NameLength3,ColumnName,NameLength4) &
      bind(C, name="SQLColumns")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char),dimension(*),intent(in) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char),dimension(*),intent(in) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      character(kind=c_char),dimension(*),intent(in) :: ColumnName
      integer(kind=c_short),intent(in),value :: NameLength4
    end function SQLColumns

    integer(kind=c_short) function SQLSetConnectAttr &
      (ConnectionHandle,Attribute,Value,StringLength) &
      bind(C, name="SQLSetConnectAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: StringLength
    end function SQLSetConnectAttr

    integer(kind=c_short) function SQLError &
      (EnvironmentHandle,ConnectionHandle,StatementHandle,Sqlstate,NativeError, &
      MessageText,BufferLength,TextLength) &
      bind(C, name="SQLError")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      type(c_ptr),intent(in),value :: ConnectionHandle
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: Sqlstate
      integer(kind=c_long),intent(out) :: NativeError
      character(kind=c_char),dimension(*),intent(in) :: MessageText
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: TextLength
    end function SQLError

    integer(kind=c_short) function SQLSetScrollOptions &
      (hstmt,fConcurrency,crowKeyset,crowRowset) &
      bind(C, name="SQLSetScrollOptions")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fConcurrency
      integer(kind=c_long),intent(in),value :: crowKeyset
      integer(kind=c_short),intent(in),value :: crowRowset
    end function SQLSetScrollOptions

    integer(kind=c_short) function TraceVSControl &
      (var1) &
      bind(C, name="TraceVSControl")
      use, intrinsic :: iso_c_binding
      integer(kind=c_long),intent(in),value :: var1
    end function TraceVSControl

    integer(kind=c_short) function SQLGetStmtAttrA &
      (hstmt,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetStmtAttrA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetStmtAttrA

    integer(kind=c_short) function SQLProcedureColumnsA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName,szColumnName,cbColumnName) &
      bind(C, name="SQLProcedureColumnsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
      character(kind=c_char),dimension(*),intent(in) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLProcedureColumnsA

    integer(kind=c_short) function SQLGetCursorName &
      (StatementHandle,CursorName,BufferLength,NameLength) &
      bind(C, name="SQLGetCursorName")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: CursorName
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: NameLength
    end function SQLGetCursorName

    integer(kind=c_short) function SQLPrepare &
      (StatementHandle,StatementText,TextLength) &
      bind(C, name="SQLPrepare")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: StatementText
      integer(kind=c_long),intent(in),value :: TextLength
    end function SQLPrepare

    integer(kind=c_short) function SQLTablePrivilegesA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLTablePrivilegesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLTablePrivilegesA

    integer(kind=c_short) function SQLAllocEnv &
      (EnvironmentHandle) &
      bind(C, name="SQLAllocEnv")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(out) :: EnvironmentHandle
    end function SQLAllocEnv

    integer(kind=c_short) function SQLTablePrivilegesW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLTablePrivilegesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLTablePrivilegesW

    integer(kind=c_short) function SQLGetInfoA &
      (hdbc,fInfoType,rgbInfoValue,cbInfoValueMax,pcbInfoValue) &
      bind(C, name="SQLGetInfoA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fInfoType
      type(c_ptr),intent(in),value :: rgbInfoValue
      integer(kind=c_short),intent(in),value :: cbInfoValueMax
      integer(kind=c_short),intent(out) :: pcbInfoValue
    end function SQLGetInfoA

    integer(kind=c_short) function SQLGetInfoW &
      (hdbc,fInfoType,rgbInfoValue,cbInfoValueMax,pcbInfoValue) &
      bind(C, name="SQLGetInfoW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fInfoType
      type(c_ptr),intent(in),value :: rgbInfoValue
      integer(kind=c_short),intent(in),value :: cbInfoValueMax
      integer(kind=c_short),intent(out) :: pcbInfoValue
    end function SQLGetInfoW

    integer(kind=c_short) function SQLConnect &
      (ConnectionHandle,ServerName,NameLength1,UserName,NameLength2, &
      Authentication,NameLength3) &
      bind(C, name="SQLConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      character(kind=c_char),dimension(*),intent(in) :: ServerName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char),dimension(*),intent(in) :: UserName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char),dimension(*),intent(in) :: Authentication
      integer(kind=c_short),intent(in),value :: NameLength3
    end function SQLConnect

    integer(kind=c_short) function SQLGetConnectAttrA &
      (hdbc,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetConnectAttrA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetConnectAttrA

    integer(kind=c_short) function SQLColAttributeW &
      (hstmt,iCol,iField,pCharAttr,cbCharAttrMax, &
      pcbCharAttr,pNumAttr) &
      bind(C, name="SQLColAttributeW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: iCol
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: pCharAttr
      integer(kind=c_short),intent(in),value :: cbCharAttrMax
      integer(kind=c_short),intent(out) :: pcbCharAttr
      integer(kind=c_long),intent(out) :: pNumAttr
    end function SQLColAttributeW

    integer(kind=c_short) function SQLColAttributes &
      (hstmt,icol,fDescType,rgbDesc,cbDescMax, &
      pcbDesc,pfDesc) &
      bind(C, name="SQLColAttributes")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(in),value :: fDescType
      type(c_ptr),intent(in),value :: rgbDesc
      integer(kind=c_short),intent(in),value :: cbDescMax
      integer(kind=c_short),intent(out) :: pcbDesc
      integer(kind=c_long),intent(out) :: pfDesc
    end function SQLColAttributes

    integer(kind=c_short) function SQLPrepareA &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLPrepareA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLPrepareA

    integer(kind=c_short) function SQLPrepareW &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLPrepareW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLPrepareW

    integer(kind=c_short) function SQLGetStmtAttrW &
      (hstmt,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetStmtAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetStmtAttrW

    integer(kind=c_short) function SQLGetDiagFieldA &
      (fHandleType,handle,iRecord,fDiagField,rgbDiagInfo, &
      cbDiagInfoMax,pcbDiagInfo) &
      bind(C, name="SQLGetDiagFieldA")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: fDiagField
      type(c_ptr),intent(in),value :: rgbDiagInfo
      integer(kind=c_short),intent(in),value :: cbDiagInfoMax
      integer(kind=c_short),intent(out) :: pcbDiagInfo
    end function SQLGetDiagFieldA

    integer(kind=c_short) function SQLGetDiagFieldW &
      (fHandleType,handle,iRecord,fDiagField,rgbDiagInfo, &
      cbDiagInfoMax,pcbDiagInfo) &
      bind(C, name="SQLGetDiagFieldW")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: fDiagField
      type(c_ptr),intent(in),value :: rgbDiagInfo
      integer(kind=c_short),intent(in),value :: cbDiagInfoMax
      integer(kind=c_short),intent(out) :: pcbDiagInfo
    end function SQLGetDiagFieldW

    integer(kind=c_short) function SQLBrowseConnect &
      (hdbc,szConnStrIn,cbConnStrIn,szConnStrOut,cbConnStrOutMax, &
      pcbConnStrOut) &
      bind(C, name="SQLBrowseConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char),dimension(*),intent(in) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char),dimension(*),intent(in) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
    end function SQLBrowseConnect

    integer(kind=c_short) function SQLExecute &
      (StatementHandle) &
      bind(C, name="SQLExecute")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLExecute

    integer(kind=c_short) function TraceOpenLogFile &
      (var1,var2,var3,var4) &
      bind(C, name="TraceOpenLogFile")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: var1
      character(kind=c_char),dimension(*),intent(in) :: var2
      character(kind=c_char),dimension(*),intent(in) :: var3
      integer(kind=c_long),intent(in),value :: var4
    end function TraceOpenLogFile

    integer(kind=c_short) function SQLExtendedFetch &
      (hstmt,fFetchType,irow,pcrow,rgfRowStatus) &
      bind(C, name="SQLExtendedFetch")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fFetchType
      integer(kind=c_long),intent(in),value :: irow
      integer(kind=c_long),intent(out) :: pcrow
      integer(kind=c_short),intent(out) :: rgfRowStatus
    end function SQLExtendedFetch

    integer(kind=c_short) function SQLDrivers &
      (henv,fDirection,szDriverDesc,cbDriverDescMax,pcbDriverDesc, &
      szDriverAttributes,cbDrvrAttrMax,pcbDrvrAttr) &
      bind(C, name="SQLDrivers")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      character(kind=c_char),dimension(*),intent(in) :: szDriverDesc
      integer(kind=c_short),intent(in),value :: cbDriverDescMax
      integer(kind=c_short),intent(out) :: pcbDriverDesc
      character(kind=c_char),dimension(*),intent(in) :: szDriverAttributes
      integer(kind=c_short),intent(in),value :: cbDrvrAttrMax
      integer(kind=c_short),intent(out) :: pcbDrvrAttr
    end function SQLDrivers

    integer(kind=c_short) function SQLSpecialColumns &
      (StatementHandle,IdentifierType,CatalogName,NameLength1,SchemaName, &
      NameLength2,TableName,NameLength3,Scope,Nullable) &
      bind(C, name="SQLSpecialColumns")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: IdentifierType
      character(kind=c_char),dimension(*),intent(in) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char),dimension(*),intent(in) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char),dimension(*),intent(in) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      integer(kind=c_short),intent(in),value :: Scope
      integer(kind=c_short),intent(in),value :: Nullable
    end function SQLSpecialColumns

    integer(kind=c_short) function SQLRowCount &
      (StatementHandle,RowCount) &
      bind(C, name="SQLRowCount")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_long),intent(out) :: RowCount
    end function SQLRowCount

    integer(kind=c_short) function SQLDescribeCol &
      (StatementHandle,ColumnNumber,ColumnName,BufferLength,NameLength, &
      DataType,ColumnSize,DecimalDigits,Nullable) &
      bind(C, name="SQLDescribeCol")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      character(kind=c_char),dimension(*),intent(in) :: ColumnName
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: NameLength
      integer(kind=c_short),intent(out) :: DataType
      integer(kind=c_long),intent(out) :: ColumnSize
      integer(kind=c_short),intent(out) :: DecimalDigits
      integer(kind=c_short),intent(out) :: Nullable
    end function SQLDescribeCol

    integer(kind=c_short) function SQLBindCol &
      (StatementHandle,ColumnNumber,TargetType,TargetValue,BufferLength, &
      StrLen_or_Ind) &
      bind(C, name="SQLBindCol")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      integer(kind=c_short),intent(in),value :: TargetType
      type(c_ptr),intent(in),value :: TargetValue
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLBindCol

    integer(kind=c_short) function SQLForeignKeysA &
      (hstmt,szPkCatalogName,cbPkCatalogName,szPkSchemaName,cbPkSchemaName, &
      szPkTableName,cbPkTableName,szFkCatalogName,cbFkCatalogName,szFkSchemaName, &
      cbFkSchemaName,szFkTableName,cbFkTableName) &
      bind(C, name="SQLForeignKeysA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szPkCatalogName
      integer(kind=c_short),intent(in),value :: cbPkCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szPkSchemaName
      integer(kind=c_short),intent(in),value :: cbPkSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szPkTableName
      integer(kind=c_short),intent(in),value :: cbPkTableName
      character(kind=c_char),dimension(*),intent(in) :: szFkCatalogName
      integer(kind=c_short),intent(in),value :: cbFkCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szFkSchemaName
      integer(kind=c_short),intent(in),value :: cbFkSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szFkTableName
      integer(kind=c_short),intent(in),value :: cbFkTableName
    end function SQLForeignKeysA

    integer(kind=c_short) function SQLGetDiagRec &
      (HandleType,Handle,RecNumber,Sqlstate,NativeError, &
      MessageText,BufferLength,TextLength) &
      bind(C, name="SQLGetDiagRec")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
      integer(kind=c_short),intent(in),value :: RecNumber
      character(kind=c_char),dimension(*),intent(in) :: Sqlstate
      integer(kind=c_long),intent(out) :: NativeError
      character(kind=c_char),dimension(*),intent(in) :: MessageText
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: TextLength
    end function SQLGetDiagRec

    integer(kind=c_short) function SQLFreeConnect &
      (ConnectionHandle) &
      bind(C, name="SQLFreeConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
    end function SQLFreeConnect

    integer(kind=c_short) function SQLGetCursorNameA &
      (hstmt,szCursor,cbCursorMax,pcbCursor) &
      bind(C, name="SQLGetCursorNameA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursorMax
      integer(kind=c_short),intent(out) :: pcbCursor
    end function SQLGetCursorNameA

    integer(kind=c_short) function SQLTablePrivileges &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLTablePrivileges")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLTablePrivileges

    integer(kind=c_short) function SQLDataSources &
      (EnvironmentHandle,Direction,ServerName,BufferLength1,NameLength1, &
      Description,BufferLength2,NameLength2) &
      bind(C, name="SQLDataSources")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      integer(kind=c_short),intent(in),value :: Direction
      character(kind=c_char),dimension(*),intent(in) :: ServerName
      integer(kind=c_short),intent(in),value :: BufferLength1
      integer(kind=c_short),intent(out) :: NameLength1
      character(kind=c_char),dimension(*),intent(in) :: Description
      integer(kind=c_short),intent(in),value :: BufferLength2
      integer(kind=c_short),intent(out) :: NameLength2
    end function SQLDataSources

    integer(kind=c_short) function SQLGetDiagField &
      (HandleType,Handle,RecNumber,DiagIdentifier,DiagInfo, &
      BufferLength,StringLength) &
      bind(C, name="SQLGetDiagField")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: DiagIdentifier
      type(c_ptr),intent(in),value :: DiagInfo
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
    end function SQLGetDiagField

    integer(kind=c_short) function SQLGetDescRec &
      (DescriptorHandle,RecNumber,Name,BufferLength,StringLength, &
      Type,SubType,Length,Precision,Scale, &
      Nullable) &
      bind(C, name="SQLGetDescRec")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      character(kind=c_char),dimension(*),intent(in) :: Name
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
      integer(kind=c_short),intent(out) :: Type
      integer(kind=c_short),intent(out) :: SubType
      integer(kind=c_long),intent(out) :: Length
      integer(kind=c_short),intent(out) :: Precision
      integer(kind=c_short),intent(out) :: Scale
      integer(kind=c_short),intent(out) :: Nullable
    end function SQLGetDescRec

    integer(kind=c_short) function SQLDescribeColA &
      (hstmt,icol,szColName,cbColNameMax,pcbColName, &
      pfSqlType,pcbColDef,pibScale,pfNullable) &
      bind(C, name="SQLDescribeColA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      character(kind=c_char),dimension(*),intent(in) :: szColName
      integer(kind=c_short),intent(in),value :: cbColNameMax
      integer(kind=c_short),intent(out) :: pcbColName
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbColDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeColA

    integer(kind=c_long) function TraceVersion &
      () &
      bind(C, name="TraceVersion")
      use, intrinsic :: iso_c_binding
    end function TraceVersion

    integer(kind=c_short) function SQLSetConnectOptionW &
      (hdbc,fOption,vParam) &
      bind(C, name="SQLSetConnectOptionW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_long),intent(in),value :: vParam
    end function SQLSetConnectOptionW

    integer(kind=c_short) function SQLBulkOperations &
      (StatementHandle,Operation) &
      bind(C, name="SQLBulkOperations")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Operation
    end function SQLBulkOperations

    integer(kind=c_short) function SQLTablesA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szTableType,cbTableType) &
      bind(C, name="SQLTablesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char),dimension(*),intent(in) :: szTableType
      integer(kind=c_short),intent(in),value :: cbTableType
    end function SQLTablesA

    integer(kind=c_short) function SQLGetTypeInfoA &
      (StatementHandle,DataTyoe) &
      bind(C, name="SQLGetTypeInfoA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: DataTyoe
    end function SQLGetTypeInfoA

    integer(kind=c_short) function SQLGetTypeInfoW &
      (StatementHandle,DataType) &
      bind(C, name="SQLGetTypeInfoW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: DataType
    end function SQLGetTypeInfoW

    integer(kind=c_short) function SQLSetPos &
      (hstmt,irow,fOption,fLock) &
      bind(C, name="SQLSetPos")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: irow
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_short),intent(in),value :: fLock
    end function SQLSetPos

    integer(kind=c_short) function SQLGetConnectOption &
      (ConnectionHandle,Option,Value) &
      bind(C, name="SQLGetConnectOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: Option
      type(c_ptr),intent(in),value :: Value
    end function SQLGetConnectOption

    integer(kind=c_short) function SQLSetStmtAttr &
      (StatementHandle,Attribute,Value,StringLength) &
      bind(C, name="SQLSetStmtAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: StringLength
    end function SQLSetStmtAttr

    integer(kind=c_short) function SQLSetDescFieldW &
      (DescriptorHandle,RecNumber,FieldIdentifier,Value,BufferLength) &
      bind(C, name="SQLSetDescFieldW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
    end function SQLSetDescFieldW

    integer(kind=c_short) function SQLFreeStmt &
      (StatementHandle,Option) &
      bind(C, name="SQLFreeStmt")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Option
    end function SQLFreeStmt

    integer(kind=c_short) function SQLGetStmtOption &
      (StatementHandle,Option,Value) &
      bind(C, name="SQLGetStmtOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Option
      type(c_ptr),intent(in),value :: Value
    end function SQLGetStmtOption

    integer(kind=c_short) function SQLFreeHandle &
      (HandleType,Handle) &
      bind(C, name="SQLFreeHandle")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
    end function SQLFreeHandle

    integer(kind=c_short) function SQLSetStmtOptionA &
      (hstmt,fOption,vParam) &
      bind(C, name="SQLSetStmtOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_long),intent(in),value :: vParam
    end function SQLSetStmtOptionA

    integer(kind=c_short) function SQLSetDescField &
      (DescriptorHandle,RecNumber,FieldIdentifier,Value,BufferLength) &
      bind(C, name="SQLSetDescField")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
    end function SQLSetDescField

    integer(kind=c_short) function SQLDriverConnectA &
      (hdbc,hwnd,szConnStrIn,cbConnStrIn,szConnStrOut, &
      cbConnStrOutMax,pcbConnStrOut,fDriverCompletion) &
      bind(C, name="SQLDriverConnectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hwnd
      character(kind=c_char),dimension(*),intent(in) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char),dimension(*),intent(in) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
      integer(kind=c_short),intent(in),value :: fDriverCompletion
    end function SQLDriverConnectA

    integer(kind=c_short) function SQLForeignKeysW &
      (hstmt,szPkCatalogName,cbPkCatalogName,szPkSchemaName,cbPkSchemaName, &
      szPkTableName,cbPkTableName,szFkCatalogName,cbFkCatalogName,szFkSchemaName, &
      cbFkSchemaName,szFkTableName,cbFkTableName) &
      bind(C, name="SQLForeignKeysW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szPkCatalogName
      integer(kind=c_short),intent(in),value :: cbPkCatalogName
      integer(kind=c_short),intent(out) :: szPkSchemaName
      integer(kind=c_short),intent(in),value :: cbPkSchemaName
      integer(kind=c_short),intent(out) :: szPkTableName
      integer(kind=c_short),intent(in),value :: cbPkTableName
      integer(kind=c_short),intent(out) :: szFkCatalogName
      integer(kind=c_short),intent(in),value :: cbFkCatalogName
      integer(kind=c_short),intent(out) :: szFkSchemaName
      integer(kind=c_short),intent(in),value :: cbFkSchemaName
      integer(kind=c_short),intent(out) :: szFkTableName
      integer(kind=c_short),intent(in),value :: cbFkTableName
    end function SQLForeignKeysW

    integer(kind=c_short) function SQLGetStmtAttr &
      (StatementHandle,Attribute,Value,BufferLength,StringLength) &
      bind(C, name="SQLGetStmtAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetStmtAttr

    integer(kind=c_short) function SQLBrowseConnectA &
      (hdbc,szConnStrIn,cbConnStrIn,szConnStrOut,cbConnStrOutMax, &
      pcbConnStrOut) &
      bind(C, name="SQLBrowseConnectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char),dimension(*),intent(in) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char),dimension(*),intent(in) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
    end function SQLBrowseConnectA

    integer(kind=c_short) function SQLParamData &
      (StatementHandle,Value) &
      bind(C, name="SQLParamData")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      type(c_ptr),intent(out) :: Value
    end function SQLParamData

    integer(kind=c_short) function SQLGetConnectAttr &
      (ConnectionHandle,Attribute,Value,BufferLength,StringLength) &
      bind(C, name="SQLGetConnectAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetConnectAttr

    integer(kind=c_short) function SQLNumResultCols &
      (StatementHandle,ColumnCount) &
      bind(C, name="SQLNumResultCols")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(out) :: ColumnCount
    end function SQLNumResultCols

    integer(kind=c_short) function SQLAllocHandle &
      (HandleType,InputHandle,OutputHandle) &
      bind(C, name="SQLAllocHandle")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: InputHandle
      type(c_ptr),intent(out) :: OutputHandle
    end function SQLAllocHandle

    integer(kind=c_short) function SQLConnectA &
      (hdbc,szDSN,cbDSN,szUID,cbUID, &
      szAuthStr,cbAuthStr) &
      bind(C, name="SQLConnectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char),dimension(*),intent(in) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSN
      character(kind=c_char),dimension(*),intent(in) :: szUID
      integer(kind=c_short),intent(in),value :: cbUID
      character(kind=c_char),dimension(*),intent(in) :: szAuthStr
      integer(kind=c_short),intent(in),value :: cbAuthStr
    end function SQLConnectA

    integer(kind=c_short) function SQLConnectW &
      (hdbc,szDSN,cbDSN,szUID,cbUID, &
      szAuthStr,cbAuthStr) &
      bind(C, name="SQLConnectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(out) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSN
      integer(kind=c_short),intent(out) :: szUID
      integer(kind=c_short),intent(in),value :: cbUID
      integer(kind=c_short),intent(out) :: szAuthStr
      integer(kind=c_short),intent(in),value :: cbAuthStr
    end function SQLConnectW

    integer(kind=c_short) function SQLNumParams &
      (hstmt,pcpar) &
      bind(C, name="SQLNumParams")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: pcpar
    end function SQLNumParams

    integer(kind=c_short) function SQLDriverConnectW &
      (hdbc,hwnd,szConnStrIn,cbConnStrIn,szConnStrOut, &
      cbConnStrOutMax,pcbConnStrOut,fDriverCompletion) &
      bind(C, name="SQLDriverConnectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hwnd
      integer(kind=c_short),intent(out) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      integer(kind=c_short),intent(out) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
      integer(kind=c_short),intent(in),value :: fDriverCompletion
    end function SQLDriverConnectW

    integer(kind=c_short) function SQLSpecialColumnsA &
      (hstmt,fColType,szCatalogName,cbCatalogName,szSchemaName, &
      cbSchemaName,szTableName,cbTableName,fScope,fNullable) &
      bind(C, name="SQLSpecialColumnsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fColType
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fScope
      integer(kind=c_short),intent(in),value :: fNullable
    end function SQLSpecialColumnsA

    integer(kind=c_short) function SQLBindParameter &
      (hstmt,ipar,fParamType,fCType,fSqlType, &
      cbColDef,ibScale,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLBindParameter")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: ipar
      integer(kind=c_short),intent(in),value :: fParamType
      integer(kind=c_short),intent(in),value :: fCType
      integer(kind=c_short),intent(in),value :: fSqlType
      integer(kind=c_long),intent(in),value :: cbColDef
      integer(kind=c_short),intent(in),value :: ibScale
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLBindParameter

    integer(kind=c_short) function SQLSpecialColumnsW &
      (hstmt,fColType,szCatalogName,cbCatalogName,szSchemaName, &
      cbSchemaName,szTableName,cbTableName,fScope,fNullable) &
      bind(C, name="SQLSpecialColumnsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fColType
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fScope
      integer(kind=c_short),intent(in),value :: fNullable
    end function SQLSpecialColumnsW

    integer(kind=c_short) function SQLGetConnectAttrW &
      (hdbc,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetConnectAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetConnectAttrW

    integer(kind=c_short) function SQLExecDirectW &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLExecDirectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLExecDirectW

    integer(kind=c_short) function SQLDisconnect &
      (ConnectionHandle) &
      bind(C, name="SQLDisconnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
    end function SQLDisconnect

    integer(kind=c_short) function SQLGetTypeInfo &
      (StatementHandle,DataType) &
      bind(C, name="SQLGetTypeInfo")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: DataType
    end function SQLGetTypeInfo

    integer(kind=c_short) function SQLStatistics &
      (StatementHandle,CatalogName,NameLength1,SchemaName,NameLength2, &
      TableName,NameLength3,Unique,Reserved) &
      bind(C, name="SQLStatistics")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char),dimension(*),intent(in) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char),dimension(*),intent(in) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      integer(kind=c_short),intent(in),value :: Unique
      integer(kind=c_short),intent(in),value :: Reserved
    end function SQLStatistics

    integer(kind=c_short) function SQLGetDescField &
      (DescriptorHandle,RecNumber,FieldIdentifier,Value,BufferLength, &
      StringLength) &
      bind(C, name="SQLGetDescField")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetDescField

    integer(kind=c_short) function SQLSetCursorName &
      (StatementHandle,CursorName,NameLength) &
      bind(C, name="SQLSetCursorName")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: CursorName
      integer(kind=c_short),intent(in),value :: NameLength
    end function SQLSetCursorName

    integer(kind=c_short) function SQLPrimaryKeysA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLPrimaryKeysA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLPrimaryKeysA

    integer(kind=c_short) function SQLDescribeColW &
      (hstmt,icol,szColName,cbColNameMax,pcbColName, &
      pfSqlType,pcbColDef,pibScale,pfNullable) &
      bind(C, name="SQLDescribeColW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(out) :: szColName
      integer(kind=c_short),intent(in),value :: cbColNameMax
      integer(kind=c_short),intent(out) :: pcbColName
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbColDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeColW

    integer(kind=c_short) function SQLAllocConnect &
      (EnvironmentHandle,ConnectionHandle) &
      bind(C, name="SQLAllocConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      type(c_ptr),intent(out) :: ConnectionHandle
    end function SQLAllocConnect

    integer(kind=c_short) function SQLDriverConnect &
      (hdbc,hwnd,szConnStrIn,cbConnStrIn,szConnStrOut, &
      cbConnStrOutMax,pcbConnStrOut,fDriverCompletion) &
      bind(C, name="SQLDriverConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hwnd
      character(kind=c_char),dimension(*),intent(in) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char),dimension(*),intent(in) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
      integer(kind=c_short),intent(in),value :: fDriverCompletion
    end function SQLDriverConnect

    integer(kind=c_int) function ODBCSetTryWaitValue &
      (dwValue) &
      bind(C, name="ODBCSetTryWaitValue")
      use, intrinsic :: iso_c_binding
      integer(kind=c_long),intent(in),value :: dwValue
    end function ODBCSetTryWaitValue

    integer(kind=c_short) function SQLProcedureColumnsW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName,szColumnName,cbColumnName) &
      bind(C, name="SQLProcedureColumnsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
      integer(kind=c_short),intent(out) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLProcedureColumnsW

    integer(kind=c_short) function SQLCopyDesc &
      (SourceDescHandle,TargetDescHandle) &
      bind(C, name="SQLCopyDesc")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: SourceDescHandle
      type(c_ptr),intent(in),value :: TargetDescHandle
    end function SQLCopyDesc

    integer(kind=c_short) function SQLNativeSqlA &
      (hdbc,szSqlStrIn,cbSqlStrIn,szSqlStr,cbSqlStrMax, &
      pcbSqlStr) &
      bind(C, name="SQLNativeSqlA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char),dimension(*),intent(in) :: szSqlStrIn
      integer(kind=c_long),intent(in),value :: cbSqlStrIn
      character(kind=c_char),dimension(*),intent(in) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStrMax
      integer(kind=c_long),intent(out) :: pcbSqlStr
    end function SQLNativeSqlA

    integer(kind=c_short) function SQLColAttributesA &
      (hstmt,icol,fDescType,rgbDesc,cbDescMax, &
      pcbDesc,pfDesc) &
      bind(C, name="SQLColAttributesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(in),value :: fDescType
      type(c_ptr),intent(in),value :: rgbDesc
      integer(kind=c_short),intent(in),value :: cbDescMax
      integer(kind=c_short),intent(out) :: pcbDesc
      integer(kind=c_long),intent(out) :: pfDesc
    end function SQLColAttributesA

    integer(kind=c_short) function SQLSetDescRec &
      (DescriptorHandle,RecNumber,Type,SubType,Length, &
      Precision,Scale,Data,StringLength,Indicator) &
      bind(C, name="SQLSetDescRec")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: Type
      integer(kind=c_short),intent(in),value :: SubType
      integer(kind=c_long),intent(in),value :: Length
      integer(kind=c_short),intent(in),value :: Precision
      integer(kind=c_short),intent(in),value :: Scale
      type(c_ptr),intent(in),value :: Data
      integer(kind=c_long),intent(out) :: StringLength
      integer(kind=c_long),intent(out) :: Indicator
    end function SQLSetDescRec

    integer(kind=c_short) function SQLProcedures &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName) &
      bind(C, name="SQLProcedures")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
    end function SQLProcedures

    integer(kind=c_short) function SQLPutData &
      (StatementHandle,Data,StrLen_or_Ind) &
      bind(C, name="SQLPutData")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      type(c_ptr),intent(in),value :: Data
      integer(kind=c_long),intent(in),value :: StrLen_or_Ind
    end function SQLPutData

    integer(kind=c_short) function SQLColumnsA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char),dimension(*),intent(in) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnsA

    integer(kind=c_short) function SQLColumnsW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(out) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnsW

    integer(kind=c_short) function SQLSetConnectAttrW &
      (hdbc,fAttribute,rgbValue,cbValue) &
      bind(C, name="SQLSetConnectAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValue
    end function SQLSetConnectAttrW

    integer(kind=c_short) function SQLGetEnvAttr &
      (EnvironmentHandle,Attribute,Value,BufferLength,StringLength) &
      bind(C, name="SQLGetEnvAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetEnvAttr

    integer(kind=c_short) function SQLBrowseConnectW &
      (hdbc,szConnStrIn,cbConnStrIn,szConnStrOut,cbConnStrOutMax, &
      pcbConnStrOut) &
      bind(C, name="SQLBrowseConnectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(out) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      integer(kind=c_short),intent(out) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
    end function SQLBrowseConnectW

    integer(kind=c_short) function SQLColAttributesW &
      (hstmt,icol,fDescType,rgbDesc,cbDescMax, &
      pcbDesc,pfDesc) &
      bind(C, name="SQLColAttributesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(in),value :: fDescType
      type(c_ptr),intent(in),value :: rgbDesc
      integer(kind=c_short),intent(in),value :: cbDescMax
      integer(kind=c_short),intent(out) :: pcbDesc
      integer(kind=c_long),intent(out) :: pfDesc
    end function SQLColAttributesW

    integer(kind=c_short) function SQLExecDirect &
      (StatementHandle,StatementText,TextLength) &
      bind(C, name="SQLExecDirect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: StatementText
      integer(kind=c_long),intent(in),value :: TextLength
    end function SQLExecDirect

    integer(kind=c_short) function SQLGetCursorNameW &
      (hstmt,szCursor,cbCursorMax,pcbCursor) &
      bind(C, name="SQLGetCursorNameW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursorMax
      integer(kind=c_short),intent(out) :: pcbCursor
    end function SQLGetCursorNameW

    integer(kind=c_short) function SQLColumnPrivileges &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnPrivileges")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char),dimension(*),intent(in) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnPrivileges

    integer(kind=c_short) function SQLColumnPrivilegesA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnPrivilegesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char),dimension(*),intent(in) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnPrivilegesA

    integer(kind=c_short) function SQLColumnPrivilegesW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnPrivilegesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(out) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnPrivilegesW

    integer(kind=c_short) function SQLGetConnectOptionA &
      (hdbc,fOption,pvParam) &
      bind(C, name="SQLGetConnectOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      type(c_ptr),intent(in),value :: pvParam
    end function SQLGetConnectOptionA

    integer(kind=c_short) function TraceReturn &
      (var1,var2) &
      bind(C, name="TraceReturn")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: var1
      integer(kind=c_short),intent(in),value :: var2
    end function TraceReturn

    integer(kind=c_short) function SQLGetDiagRecA &
      (fHandleType,handle,iRecord,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLGetDiagRecA")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      character(kind=c_char),dimension(*),intent(in) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      character(kind=c_char),dimension(*),intent(in) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLGetDiagRecA

    integer(kind=c_short) function SQLGetDiagRecW &
      (fHandleType,handle,iRecord,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLGetDiagRecW")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(out) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      integer(kind=c_short),intent(out) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLGetDiagRecW

    integer(kind=c_short) function SQLPrimaryKeys &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLPrimaryKeys")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLPrimaryKeys

    integer(kind=c_short) function SQLSetConnectOptionA &
      (hdbc,fOption,vParam) &
      bind(C, name="SQLSetConnectOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_long),intent(in),value :: vParam
    end function SQLSetConnectOptionA

    integer(kind=c_short) function SQLForeignKeys &
      (hstmt,szPkCatalogName,cbPkCatalogName,szPkSchemaName,cbPkSchemaName, &
      szPkTableName,cbPkTableName,szFkCatalogName,cbFkCatalogName,szFkSchemaName, &
      cbFkSchemaName,szFkTableName,cbFkTableName) &
      bind(C, name="SQLForeignKeys")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szPkCatalogName
      integer(kind=c_short),intent(in),value :: cbPkCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szPkSchemaName
      integer(kind=c_short),intent(in),value :: cbPkSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szPkTableName
      integer(kind=c_short),intent(in),value :: cbPkTableName
      character(kind=c_char),dimension(*),intent(in) :: szFkCatalogName
      integer(kind=c_short),intent(in),value :: cbFkCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szFkSchemaName
      integer(kind=c_short),intent(in),value :: cbFkSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szFkTableName
      integer(kind=c_short),intent(in),value :: cbFkTableName
    end function SQLForeignKeys

    integer(kind=c_short) function SQLSetConnectAttrA &
      (hdbc,fAttribute,rgbValue,cbValue) &
      bind(C, name="SQLSetConnectAttrA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValue
    end function SQLSetConnectAttrA

    integer(kind=c_short) function SQLDriversA &
      (henv,fDirection,szDriverDesc,cbDriverDescMax,pcbDriverDesc, &
      szDriverAttributes,cbDrvrAttrMax,pcbDrvrAttr) &
      bind(C, name="SQLDriversA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      character(kind=c_char),dimension(*),intent(in) :: szDriverDesc
      integer(kind=c_short),intent(in),value :: cbDriverDescMax
      integer(kind=c_short),intent(out) :: pcbDriverDesc
      character(kind=c_char),dimension(*),intent(in) :: szDriverAttributes
      integer(kind=c_short),intent(in),value :: cbDrvrAttrMax
      integer(kind=c_short),intent(out) :: pcbDrvrAttr
    end function SQLDriversA

    integer(kind=c_short) function SQLColAttributeA &
      (hstmt,iCol,iField,pCharAttr,cbCharAttrMax, &
      pcbCharAttr,pNumAttr) &
      bind(C, name="SQLColAttributeA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: iCol
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: pCharAttr
      integer(kind=c_short),intent(in),value :: cbCharAttrMax
      integer(kind=c_short),intent(out) :: pcbCharAttr
      integer(kind=c_long),intent(out) :: pNumAttr
    end function SQLColAttributeA

    integer(kind=c_short) function SQLDriversW &
      (henv,fDirection,szDriverDesc,cbDriverDescMax,pcbDriverDesc, &
      szDriverAttributes,cbDrvrAttrMax,pcbDrvrAttr) &
      bind(C, name="SQLDriversW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      integer(kind=c_short),intent(out) :: szDriverDesc
      integer(kind=c_short),intent(in),value :: cbDriverDescMax
      integer(kind=c_short),intent(out) :: pcbDriverDesc
      integer(kind=c_short),intent(out) :: szDriverAttributes
      integer(kind=c_short),intent(in),value :: cbDrvrAttrMax
      integer(kind=c_short),intent(out) :: pcbDrvrAttr
    end function SQLDriversW

    integer(kind=c_short) function SQLProcedureColumns &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName,szColumnName,cbColumnName) &
      bind(C, name="SQLProcedureColumns")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
      character(kind=c_char),dimension(*),intent(in) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLProcedureColumns

    integer(kind=c_short) function SQLFetch &
      (StatementHandle) &
      bind(C, name="SQLFetch")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLFetch

    integer(kind=c_short) function SQLSetStmtOption &
      (StatementHandle,Option,Value) &
      bind(C, name="SQLSetStmtOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Option
      integer(kind=c_long),intent(in),value :: Value
    end function SQLSetStmtOption

    integer(kind=c_short) function SQLProceduresA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName) &
      bind(C, name="SQLProceduresA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
    end function SQLProceduresA

    integer(kind=c_short) function SQLGetDescRecA &
      (hdesc,iRecord,szName,cbNameMax,pcbName, &
      pfType,pfSubType,pLength,pPrecision,pScale, &
      pNullable) &
      bind(C, name="SQLGetDescRecA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      character(kind=c_char),dimension(*),intent(in) :: szName
      integer(kind=c_short),intent(in),value :: cbNameMax
      integer(kind=c_short),intent(out) :: pcbName
      integer(kind=c_short),intent(out) :: pfType
      integer(kind=c_short),intent(out) :: pfSubType
      integer(kind=c_long),intent(out) :: pLength
      integer(kind=c_short),intent(out) :: pPrecision
      integer(kind=c_short),intent(out) :: pScale
      integer(kind=c_short),intent(out) :: pNullable
    end function SQLGetDescRecA

    integer(kind=c_short) function SQLGetDescRecW &
      (hdesc,iRecord,szName,cbNameMax,pcbName, &
      pfType,pfSubType,pLength,pPrecision,pScale, &
      pNullable) &
      bind(C, name="SQLGetDescRecW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(out) :: szName
      integer(kind=c_short),intent(in),value :: cbNameMax
      integer(kind=c_short),intent(out) :: pcbName
      integer(kind=c_short),intent(out) :: pfType
      integer(kind=c_short),intent(out) :: pfSubType
      integer(kind=c_long),intent(out) :: pLength
      integer(kind=c_short),intent(out) :: pPrecision
      integer(kind=c_short),intent(out) :: pScale
      integer(kind=c_short),intent(out) :: pNullable
    end function SQLGetDescRecW

    integer(kind=c_short) function TraceCloseLogFile &
      (var1) &
      bind(C, name="TraceCloseLogFile")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: var1
    end function TraceCloseLogFile

    integer(kind=c_short) function SQLSetCursorNameA &
      (hstmt,szCursor,cbCursor) &
      bind(C, name="SQLSetCursorNameA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursor
    end function SQLSetCursorNameA

    integer(kind=c_short) function SQLGetStmtOptionA &
      (hstmt,fOption,pvParam) &
      bind(C, name="SQLGetStmtOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fOption
      type(c_ptr),intent(in),value :: pvParam
    end function SQLGetStmtOptionA

    integer(kind=c_short) function SQLErrorA &
      (henv,hdbc,hstmt,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLErrorA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      character(kind=c_char),dimension(*),intent(in) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLErrorA

    integer(kind=c_short) function SQLErrorW &
      (henv,hdbc,hstmt,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLErrorW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      integer(kind=c_short),intent(out) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLErrorW

    integer(kind=c_short) function SQLSetCursorNameW &
      (hstmt,szCursor,cbCursor) &
      bind(C, name="SQLSetCursorNameW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursor
    end function SQLSetCursorNameW

    integer(kind=c_short) function SQLProceduresW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName) &
      bind(C, name="SQLProceduresW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
    end function SQLProceduresW

    integer(kind=c_short) function SQLDescribeParam &
      (hstmt,ipar,pfSqlType,pcbParamDef,pibScale, &
      pfNullable) &
      bind(C, name="SQLDescribeParam")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: ipar
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbParamDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeParam

    integer(kind=c_short) function SQLColAttribute &
      (StatementHandle,ColumnNumber,FieldIdentifier,CharacterAttribute,BufferLength, &
      StringLength,NumericAttribute) &
      bind(C, name="SQLColAttribute")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: CharacterAttribute
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
      integer(kind=c_long),intent(out) :: NumericAttribute
    end function SQLColAttribute

    integer(kind=c_short) function SQLAllocHandleStd &
      (fHandleType,hInput,phOutput) &
      bind(C, name="SQLAllocHandleStd")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: hInput
      type(c_ptr),intent(out) :: phOutput
    end function SQLAllocHandleStd

    integer(kind=c_short) function SQLSetConnectOption &
      (ConnectionHandle,Option,Value) &
      bind(C, name="SQLSetConnectOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: Option
      integer(kind=c_long),intent(in),value :: Value
    end function SQLSetConnectOption

    integer(kind=c_short) function SQLMoreResults &
      (hstmt) &
      bind(C, name="SQLMoreResults")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
    end function SQLMoreResults

    integer(kind=c_short) function SQLGetData &
      (StatementHandle,ColumnNumber,TargetType,TargetValue,BufferLength, &
      StrLen_or_Ind) &
      bind(C, name="SQLGetData")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      integer(kind=c_short),intent(in),value :: TargetType
      type(c_ptr),intent(in),value :: TargetValue
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLGetData

    integer(kind=c_short) function SQLTablesW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szTableType,cbTableType) &
      bind(C, name="SQLTablesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(out) :: szTableType
      integer(kind=c_short),intent(in),value :: cbTableType
    end function SQLTablesW

    integer(kind=c_short) function SQLCancel &
      (StatementHandle) &
      bind(C, name="SQLCancel")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLCancel

    integer(kind=c_short) function SQLStatisticsA &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,fUnique,fAccuracy) &
      bind(C, name="SQLStatisticsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char),dimension(*),intent(in) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char),dimension(*),intent(in) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char),dimension(*),intent(in) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fUnique
      integer(kind=c_short),intent(in),value :: fAccuracy
    end function SQLStatisticsA

    integer(kind=c_short) function SQLStatisticsW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,fUnique,fAccuracy) &
      bind(C, name="SQLStatisticsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fUnique
      integer(kind=c_short),intent(in),value :: fAccuracy
    end function SQLStatisticsW

    integer(kind=c_short) function SQLDescribeParamA &
      (hstmt,ipar,pfSqlType,pcbParamDef,pibScale, &
      pfNullable) &
      bind(C, name="SQLDescribeParamA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: ipar
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbParamDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeParamA

    integer(kind=c_short) function SQLGetDescFieldA &
      (hdesc,iRecord,iField,rgbValue,cbValueMax, &
      pcbValue) &
      bind(C, name="SQLGetDescFieldA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetDescFieldA

    integer(kind=c_short) function SQLPrimaryKeysW &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLPrimaryKeysW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLPrimaryKeysW

    integer(kind=c_short) function SQLGetDescFieldW &
      (hdesc,iRecord,iField,rgbValue,cbValueMax, &
      pcbValue) &
      bind(C, name="SQLGetDescFieldW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetDescFieldW

    integer(kind=c_short) function SQLNativeSqlW &
      (hdbc,szSqlStrIn,cbSqlStrIn,szSqlStr,cbSqlStrMax, &
      pcbSqlStr) &
      bind(C, name="SQLNativeSqlW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(out) :: szSqlStrIn
      integer(kind=c_long),intent(in),value :: cbSqlStrIn
      integer(kind=c_short),intent(out) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStrMax
      integer(kind=c_long),intent(out) :: pcbSqlStr
    end function SQLNativeSqlW

    integer(kind=c_short) function SQLTables &
      (StatementHandle,CatalogName,NameLength1,SchemaName,NameLength2, &
      TableName,NameLength3,TableType,NameLength4) &
      bind(C, name="SQLTables")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char),dimension(*),intent(in) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char),dimension(*),intent(in) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char),dimension(*),intent(in) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      character(kind=c_char),dimension(*),intent(in) :: TableType
      integer(kind=c_short),intent(in),value :: NameLength4
    end function SQLTables

    integer(kind=c_short) function SQLBindParam &
      (StatementHandle,ParameterNumber,ValueType,ParameterType,LengthPrecision, &
      ParameterScale,ParameterValue,StrLen_or_Ind) &
      bind(C, name="SQLBindParam")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ParameterNumber
      integer(kind=c_short),intent(in),value :: ValueType
      integer(kind=c_short),intent(in),value :: ParameterType
      integer(kind=c_long),intent(in),value :: LengthPrecision
      integer(kind=c_short),intent(in),value :: ParameterScale
      type(c_ptr),intent(in),value :: ParameterValue
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLBindParam

    integer(kind=c_short) function SQLAllocStmt &
      (ConnectionHandle,StatementHandle) &
      bind(C, name="SQLAllocStmt")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      type(c_ptr),intent(out) :: StatementHandle
    end function SQLAllocStmt

    integer(kind=c_short) function SQLTransact &
      (EnvironmentHandle,ConnectionHandle,CompletionType) &
      bind(C, name="SQLTransact")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: CompletionType
    end function SQLTransact

  end interface
end module fodbc
