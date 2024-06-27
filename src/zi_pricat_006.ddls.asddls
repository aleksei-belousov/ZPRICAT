@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_PRICAT_006'
define root view entity ZI_PRICAT_006 as select from zpricat006 as Pricat
composition [0..*] of ZI_PRODUCT_006 as _Product
composition [0..*] of ZI_CUSTOMER_006 as _Customer
association [0..1] to I_SalesOrganization as _SalesOrganization on $projection.SalesOrganization = _SalesOrganization.SalesOrganization
association [0..1] to I_DistributionChannel as _DistributionChannel on $projection.DistributionChannel = _DistributionChannel.DistributionChannel
association [0..1] to I_Division as _Division on $projection.Division = _Division.Division
{
    key pricatuuid as PricatUUID,
    
    pricatid as PricatID,
    salesorganization as SalesOrganization,
    distributionchannel as DistributionChannel,
    division as Division,
    pricingdate as PricingDate,
    released as Released,
    
    productseries as ProductSeries,
    salesstatus as SalesStatus,
    producttypes as ProductTypes,
    colors as Colors,
    collections as Collections,
    rowlimit as RowLimit,

    createdby as CreatedBy,
    createdat as CreatedAt,
    lastchangedby as LastChangedBy,
    lastchangedat as LastChangedAt,
    locallastchangedat as LocalLastChangedAt,

    /* Associations */
    _Product,
    _Customer,
    _SalesOrganization,
    _DistributionChannel,
    _Division
}
