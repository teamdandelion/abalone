module.exports = function(grunt) {
  "use strict";

  var path = require("path");
  var cwd = process.cwd();

  var tsJSON = {
    dev: {
      src: ["src/**/*.ts", "typings/**/*.d.ts"],
      out: "build/abalone.js",
      options: {
        declaration: true
      }
    },
    test: {
      src: ["test/*.ts", "typings/**/*.d.ts", "build/**/*.d.ts"],
      out: "build/test.js",
    }
  };


  var configJSON = {
    pkg: grunt.file.readJSON("package.json"),
    ts: tsJSON,
    tslint: {
      options: {
        configuration: grunt.file.readJSON("tslint.json")
      },
      files: ["src/**/*.ts", "test/**.ts"]
    },
    watch: {
      "options": {
        livereload: true
      },
      "rebuild": {
        "tasks": ["default"],
        "files": ["src/**/*.ts", "test/**/*.ts"]
      }
    },
    mocha: {
      test: {
        src: ['test/test.html'],
        options: {
          run: true
        }
      }
    },
    connect: {
      server: {
        options: {
          port: 9999,
          base: "",
          livereload: true
        }
      }
    },
    // clean: {
    //   tscommand: ["tscommand*.tmp.txt"]
    // },
  };


  // project configuration
  grunt.initConfig(configJSON);

  require('load-grunt-tasks')(grunt);

  // default task (this is what runs when a task isn't specified)
  grunt.registerTask("default", [
                                  "ts:dev",
                                  "ts:test",
                                  "tslint",
                                  "mocha"]);

  grunt.registerTask("launch", ["connect", "default", "watch"]);
};
