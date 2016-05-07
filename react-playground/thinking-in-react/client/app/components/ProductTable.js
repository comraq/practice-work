import React from "react";

import ProductCategoryRow from "./ProductCategoryRow";
import ProductRow from "./ProductRow";

class ProductTable extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    let category = null;
    let tableBody = this.props.data.map((item, i) => {
      if (item.category !== category) {
        category = item.category;
        return <ProductCategoryRow category={ item.category }
                 key={ i } />;
      }

      return <ProductRow name={ item.name } price={ item.price }
               key={ i } />;
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
