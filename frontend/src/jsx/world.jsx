var WorldMessage = React.createClass({
  render: function() {
    return <div>Hello {this.props.name}</div>;
  }
});

React.render(<WorldMessage name="John" />, mountNode);
