import React from "react";

class SearchBar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      searchText: "",
      inStock: false
    };

    this.searchChanged = this.searchChanged.bind(this);
  }

  searchChanged() {
    console.log("searchChanged");
  }

  render() {
    return <div>
      <form>
        <input type="text" placeholder="Search..."
          value={ this.state.searchText } onChange={ this.searchChanged } />
        <br />
        <input type="checkbox" checked={ this.state.inStock } />
          Only show products in stock<br/>
      </form>
    </div>;
  }
}

export default SearchBar;
