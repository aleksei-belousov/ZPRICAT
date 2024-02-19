@EndUserText.label: 'ZC_CUSTOMER_006'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_CUSTOMER_006 as projection on ZI_CUSTOMER_006
{
    key CustomerUUID,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Customer', element: 'Customer' } } ] 
    @EndUserText.label: 'Outbound Delivery'
    CustomerID,

    PricatUUID,
    Currency, 
    Country,
    GLN,
    CustomerURL,

    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    
    /* Associations */
    _Pricat : redirected to parent ZC_PRICAT_006

}
