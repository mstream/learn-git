const Main = require('./output/Main');

const main = () => {
  Main.main();
}

if (module.hot) {
  module.hot.accept(() => {
    console.log('Reloaded, running main again');
    main();
  });
}

console.log('Starting app');
main();
