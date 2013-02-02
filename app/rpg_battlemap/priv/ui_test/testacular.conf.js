basePath = '../www/';

files = [
  JASMINE,
  JASMINE_ADAPTER,
  'contrib/angular.js',
  'contrib/angular-*.js',
  'contrib/angular-mocks.js',
  'contrib/jquery.js',
  'app/'
  //'js/**/*.js'
//  'app/lib/angular/angular.js',
//  'app/lib/angular/angular-*.js',
//  'test/lib/angular/angular-mocks.js',
//  'app/js/**/*.js',
//  'test/unit/**/*.js'
];

autoWatch = true;

browsers = ['Chrome','PhantomJS'];

junitReporter = {
  outputFile: 'test_out/unit.xml',
  suite: 'unit'
};
