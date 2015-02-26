var superagent = require('superagent')
var Flux = require('flux')
var Dispatcher = Flux.Dispatcher;
var assign = require('object-assign');
var keyMirror = require('keymirror');
var EventEmitter = require('events').EventEmitter;
var CHANGE_EVENT = 'change';

var UIDispatcher = assign(new Dispatcher(), {
  handleAction: function(action) {
    var payload = {
      action: action
    };
    this.dispatch(payload);
  }
});

var UIConstants = {
  ActionTypes: keyMirror({
    IMAGES_UPLOAD_SUBMIT_FORM: null,
  }),
};
var ActionTypes = UIConstants.ActionTypes;

var ImagesStore = assign({loading: false}, EventEmitter.prototype, {
  emitChange: function() {
    this.emit(CHANGE_EVENT);
  },
  addChangeListener: function(callback) {
    this.on(CHANGE_EVENT, callback);
  },
  removeChangeListener: function(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  },
  getState: function() {
    return {
      loading: this.loading
    }
  },
});


var AuthStore = assign({
  emailAddress: "",
  token: "",
  isLoggedIn: false
}, EventEmitter.prototype, {
  emitChange: function() {
    this.emit(CHANGE_EVENT);
  },
  addChangeListener: function(callback) {
    this.on(CHANGE_EVENT, callback);
  },
  removeChangeListener: function(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  },
  getState: function() {
    return {
      emailAddress: this.emailAddress,
      token: this.token,
      isLoggedIn: this.isLoggedIn
    }
  },
});

AuthStore.dispatchToken = UIDispatcher.register(function(payload) {
  var action = payload.action;
  switch(action.type) {
    default:
  }
});

ImagesStore.dispatchToken = UIDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.type) {
    case ActionTypes.IMAGES_UPLOAD_SUBMIT_FORM:
      ImagesStore.loading = true;
      superagent
        .post(action.data.url)
        .send({
          source: action.data.source,
          image: action.data.image
        })
        .set('Accept', 'application/json')
        .end(function(err, res) {
          console.log(res.status);
          ImagesStore.loading = false;
          ImagesStore.emitChange();
        });
      ImagesStore.emitChange();
      break;

    default:
  }
});

module.exports = {
  ImagesStore: ImagesStore,
  AuthStore: AuthStore,
  UIDispatcher: UIDispatcher
};
