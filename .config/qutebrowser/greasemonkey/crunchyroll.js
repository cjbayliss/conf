// ==UserScript==
// @name         crunchyroll
// @namespace    https://github.com/cjbaliss/
// @version      0.1
// @description  better crunchyroll theme
// @author       Christopher Bayliss
// @match        https://www.crunchyroll.com/*
// @grant        none
// ==/UserScript==

const styles = `
body,
.header-searchbox,
.header-searchbox-submit,
.header-search-autocomplete,
.main-tabs,
.episode-progress-bar,
.rating-bar {
    background: #282a36 !important;
    color: #f2f2f2;
}
#template_container.template-container {
    background: unset;
}
.new-template-body h3 {
    padding: unset;
    border: unset;
}
.new-template-body,
#showmedia_about_info_details span,
.header_search_result_details,
.header_search_result_name,
.header_search_result_type {
    color: #f2f2f2;
}
a {
    color: #bd93f9 !important;
}
a:hover {
    color: #f47521 !important;
}
.header-userpanel .header-icon {
    fill: #bd93f9;
}
.availability-notes-low,
.white-wrapper.container-shadow.large-margin-bottom,
header,
#footer_menu,
#tabs,
.main-tabs a.selected,
.main-tabs a.selected:hover,
.main-tabs a:hover,
.landscape-element.block-link.cf.titlefix:hover,
.portrait-element:hover,
.load-more:hover,
.search-item:hover,
.availability-notes-high,
#template_body.old-template-body {
    background: #363949 !important;
}
.header-search-form,
.header-search-autocomplete,
.welcome-block h1,
.welcome-title-list h1,
.welcome-crnews-item,
.welcome-news-item,
#welcome_left,
#sidebar,
.welcome-countdown-day {
    border-color: #414458;
}
.site-header {
    box-shadow: unset;
}
.main-tabs {
    padding: unset;
}
.main-tabs a,
.portrait-element,
.default-button,
a.default-button,
input.default-button,
span.default-button {
    background: #303241;
    text-shadow: unset;
    border: unset;
}
#source_showview .season-dropdown.open,
#source_showview .season-dropdown {
    background-color: #303241;
    border: unset;
}
#source_showview .season-dropdown.open:hover,
#source_showview .season-dropdown:hover {
    background-color: #363949;
}
img.portrait,
.header_search_result_mug,
img {
    border: unset !important;
}
.header-search-autocomplete a {
    border-color: #414458;
    border-right: unset;
}
.js-add-queue-button,
.share-buttons,
#marketing_banner,
#message_box,
#showmedia_free_trial_signup,
.game-banner-wrapper,
.showmedia-submenu,
.comments,
.follow-crunchyroll-social,
#showmedia_about_info_avail,
.games,
.store,
.try-free-header-menu-item,
.login,
.queue,
#ot-sdk-btn,
#social_media,
.trunc-desc,
.more-dots {
    display: none !important;
}
.more {
    display: unset !important;
}
#sidebar {
    width: unset;
    margin-top: 10rem;
}
.right {
    float: unset;
    display: unset;
}
#sidebar.right {
    display: inline-flex !important;
}
.showmedia-related {
    padding-left: 1rem;
}
#main_content,
.player-container-16-9 {
    width: unset;
    height: 540px;
    float: unset;
}
#showmedia_about_info_details,
h6,
.series-data,
.small-data {
    color: #c0c0c0 !important;
}
.portrait-grid li {
    margin-left: 10.5px;
}
#source_showview .season-dropdown {
    background-position: 930px -11px;
}
#source_showview .season-dropdown.open {
    background-position: 930px 12px;
}
`;

let styleSheet = document.createElement("style");
styleSheet.type = "text/css";
styleSheet.innerText = styles;
document.head.appendChild(styleSheet);
