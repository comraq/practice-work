Another clone of my gulp boilerplate for react router tutorials at:      
https://github.com/reactjs/react-router-tutorial

Commands:     
npm install - installs all dependencies listed in package.json      
npm start - starts the local backend server      

npm run build - transpiles/builds all necessary files for production, without sourcemaps     
npm run buildd - transpiles/builds all necessary files, with sourcemaps     

npm run watchd - transpiles/builds all with sourcemaps, then watches for
src js and css changes and auto rebuild           
npm run watchrs - transpiles/builds all with sourcemaps, then watches for src js and css changes and auto rebuild and live reloads browser with a static server            
npm run watchr - transpiles/builds all with sourcemaps, then watches for src js and css changes and auto rebuild and live reloads browser with server started by "npm start" (must run "npm start" before this)          

npm test - watches for src js changes, then run sanity testSuite       

NOTE: browser-sync does not work well with sudo!    
https://www.browsersync.io/docs/
