@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_CUSTOMER_006'
define view entity ZI_CUSTOMER_006 as select from zcustomer006 as Customer
association to parent ZI_PRICAT_006 as _Pricat on $projection.PricatUUID = _Pricat.PricatUUID
{
    key customeruuid as CustomerUUID,
    customerid as CustomerID,
    pricatuuid as PricatUUID,
    currency as Currency, 
    country as Country,
    gln as GLN,
    createdby as CreatedBy,
    createdat as CreatedAt,
    lastchangedby as LastChangedBy,
    lastchangedat as LastChangedAt,
    locallastchangedat as LocalLastChangedAt,
    _Pricat
}
