var React = require('react')
var Table = require('react-bootstrap').Table

var ReplayChooser = React.createClass({
  getInitialState: function() {
    return {data: []};
  },
  loadGamesFromServer: function() {
    $.ajax({
      url: '/api/games',
      dataType: 'json',
      success: function(data) {
        this.setState({data: data});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error("erorr getting api/games", status, err.toString());
      }.bind(this)
    })
  },
  componentDidMount: function() {
    this.loadGamesFromServer()
  },
  render: function() {
    return (
        <div className="replayChooser">
        <h1> Choose a Game </h1>
        <GameList data={this.state.data}/>
        </div>
      );
  }
});

var GameList = React.createClass({
  render: function() {
    var gameNodes = this.props.data.map(function (game) {
      var dest = "/viewGame/" + game.ID;
      return (
        <tr>
          <td>
            <a href={dest}>
              <i className="fa fa-youtube-play"> </i>
            </a>
          </td>
          <td>{game.ID}</td>
          <td>{game.Status}</td>
          <td>{game.Reason}</td>
          <td>{game.WhitePlayer.Name} v{game.WhitePlayer.Version}</td>
          <td>{game.BlackPlayer.Name} v{game.BlackPlayer.Version}</td>
        </tr>
      );
    });
    return (
      <div className="gameList">
        <Table>
          <thead>
            <tr>
              <th></th>
              <th>#</th>
              <th>Result</th>
              <th>Info</th>
              <th>White</th>
              <th>Black</th>
            </tr>
          </thead>
          <tbody>
            {gameNodes}
          </tbody>
        </Table>
      </div>
    );
  }
});

module.exports = ReplayChooser;
