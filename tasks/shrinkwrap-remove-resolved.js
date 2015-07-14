var fs = require('fs');
var shrinkwrap = require('../npm-shrinkwrap.json');

function replacer(key, val) {
    if (key === 'resolved' && this.from && this.version) {
        return undefined;
    } else {
        return val;
    }
}

fs.writeFileSync('npm-shrinkwrap.json', JSON.stringify(shrinkwrap, replacer, 2));
