module.exports = function(grunt) {

  grunt.initConfig({

    jade: {
      compile: {
        files: {
          "index.html": ["index.jade"]
        }
      }
    },

    sass: {                              
      dist: {                           
        files: {                        
          'css/theme/data-mining.css': 'css/theme/source/data-mining.scss'
        }
      }
    },

    connect: {
      dev: {
        port: 8000
      },
      livereload: {
        options: {
          hostname: 'localhost'
        }
      }
    },

    open: {
      all: {
        path: 'http://localhost:8000'
      }
    },

    watch: {
      index: {
        files: ['index.jade', 'index.html'],
        tasks: ['jade'],
        options: {
          spawn: false,
          livereload: true
        },
      },
      css: {
        files:  ['css/theme/**/*.scss'],
        tasks:  ['sass'],
        options: {
          spawn: false,
          livereload: true
        }
      }
    }

  });

   // Load Grunt tasks declared in the package.json file
  require('matchdep').filterDev('grunt-*').forEach(grunt.loadNpmTasks);

  grunt.registerTask('build', ['jade', 'sass']);
  grunt.registerTask('serve', ['build', 'connect', 'watch']);

};
