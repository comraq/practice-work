import React from "react";

import SearchBar from "./SearchBar";
import ProductTable from "./ProductTable";

class FilterableProductTable extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      filter: {
        text: "",
        inStock: false
      }
    };

    this.filterTextChanged = this.filterTextChanged.bind(this);
    this.inStockChanged = this.inStockChanged.bind(this);
  }

  filterTextChanged(text) {
    this.setState({
      filter: {
        text: text,
        inStock: this.state.filter.inStock
      }
    });
  }

  inStockChanged(inStock) {
    this.setState({
      filter: {
        text: this.state.filter.text,
        inStock: inStock
      }
    });
  }

  render() {
    return <div>
      <SearchBar applyTextFilter={ this.filterTextChanged }
        applyInStockFilter={ this.inStockChanged } />
      <ProductTable data={ this.props.data }
        filter={ this.state.filter } />
    </div>;
  }
}

export default FilterableProductTable;
