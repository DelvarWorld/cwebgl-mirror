/** @jsx React.DOM */
React.renderComponent(
    React.DOM.div( {class:"row"}, 
        React.DOM.div( {class:"span4"}, "..."),
        React.DOM.div( {class:"span8"}, "...")
    ),
    document.getElementById('example')
);
