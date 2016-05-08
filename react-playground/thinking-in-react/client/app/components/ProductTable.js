import React from "react";

import ProductCategoryRow from "./ProductCategoryRow";
import ProductRow from "./ProductRow";

class ProductTable extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    let tableBody = [];

    let category = null;
    let inStockOnly = this.props.filter.inStock;
    let search = this.props.filter.text;

    let filteredData = this.props.data.filter(item => {
                      if (inStockOnly && !item.stocked) 
                        return false;
   
                      if (item.name.toLowerCase().slice(0, search.length)
                            != search.toLowerCase())
                        return false;

                      return true;
                    })
                    .forEach((item, i) => {
                      if (item.category !== category) {
                        category = item.category;
                        tableBody.push(
                          <ProductCategoryRow
                            category={ item.category }
                            key={ "category " + i } />
                        );
                      }

                      tableBody.push(
                        <ProductRow name={ item.name }
                          price={ item.price }
                          key={ i } />
                      );
                    });

    return <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Price</th>
        </tr>
      </thead>
      <tbody>
        { tableBody }
      </tbody>
    </table>;
  }
}

export default ProductTable;
