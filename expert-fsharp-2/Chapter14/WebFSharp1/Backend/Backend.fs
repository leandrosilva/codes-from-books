namespace FSharpWeb

open System.Configuration
open Microsoft.FSharp.Linq.Query

type Category =
    val mutable _Name : string
    val mutable _CategoryID : int

    member this.Name
        with get () = this._Name
        and  set name = this._Name <- name

    member this.CategoryID
        with get () = this._CategoryID
        and  set name = this._CategoryID <- name

    new (categoryID, name) = { _CategoryID=categoryID; _Name=name }

type Product =
    val mutable _Name : string
    val mutable _Price : System.Decimal

    member this.Name
        with get () = this._Name
        and  set name = this._Name <- name

    member this.Price
        with get () = this._Price
        and  set name = this._Price <- name

    new (name, price) = { _Name=name; _Price=price }

module DataSource =
    let cs = ConfigurationManager.ConnectionStrings
                .Item("AdventureWorks").ConnectionString
    let db = new Dao.DaoDataContext(cs)
    let GetCategories () =
        query <@ seq { for c in db.ProductCategories
                       -> new Category(c.ProductCategoryID, c.Name) } @>

    let GetProducts categoryId =
        query <@ seq {  for p in db.Products do
                        if p.ProductCategoryID = categoryId then
                            yield new Product(p.Name, p.ListPrice) } @>
