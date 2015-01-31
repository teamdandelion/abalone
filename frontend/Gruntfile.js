module.exports = function(grunt) {
  "use strict";

  var path = require("path");
  var cwd = process.cwd();

  var tsJSON = {
    dev: {
      src: ["src/**/*.ts", "typings/**/*.d.ts"],
      outDir: "build/src/",
    },
    test: {
      src: ["test/*.ts", "typings/**/*.d.ts", "build/**/*.d.ts"],
      outDir: "build/test/",
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
    blanket_mocha: {
      all: ['test/coverage.html'],
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
                                  "tslint",
                                  "blanket_mocha"]);

  grunt.registerTask("launch", ["connect", "default", "watch"]);
};
