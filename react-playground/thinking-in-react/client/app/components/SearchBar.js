import React from "react";

class SearchBar extends React.Component {
  constructor(props) {
    super(props);

    this.textChanged = this.textChanged.bind(this);
    this.inStockChanged = this.inStockChanged.bind(this);
  }

  textChanged(event) {
    this.props.applyTextFilter(event.target.value);
  }

  inStockChanged(event) {
    this.props.applyInStockFilter(event.target.checked);
  }

  render() {
    return <div>
      <form>
        <input type="text" placeholder="Search..."
          onChange={ this.textChanged } />
        <br />
        <input type="checkbox"
          onChange={ this.inStockChanged } />
          Only show products in stock<br/>
      </form>
    </div>;
  }
}

export default SearchBar;
