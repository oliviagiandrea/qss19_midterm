const graphsData = ` [
  {
    "title": "Frequency of Work by Hour and Sex",
    "img": "./graphs/freq_work.png"
  },
  {
    "title": "Frequency of Education by Hour and Sex",
    "img": "./graphs/freq_educ.png"
  },
  {
    "title": "Frequency of Leisure by Hour and Sex",
    "img": "./graphs/freq_leisure.png"
  },
  {
    "title": "Frequency of Personal Care by Hour and Sex",
    "img": "./graphs/freq_care.png"
  },
  
  {
    "title": "Average Work Time (Hours) by State",
    "img": "./graphs/choropleth_work.png"
  },
  {
    "title": "Average Education Time (Hours) by State",
    "img": "./graphs/choropleth_educ.png"
  },
  {
    "title": "Average Leisure Time (Hours) by State",
    "img": "./graphs/choropleth_leisure.png"
  },
  {
    "title": "Average Personal Care Time (Hours) by State",
    "img": "./graphs/choropleth_care.png"
  },

  {
    "title": "Gender Distribution",
    "img": "./graphs/pie_gender.png"
  },
  {
    "title": "Race Distribution",
    "img": "./graphs/pie_race.png"
  },
  {
    "title": "Income Distribution",
    "img": "./graphs/pie_inc.png"
  },
  {
    "title": "Employment Distribution",
    "img": "./graphs/pie_emp.png"
  },

  {
    "title": "Time Spent in Categories by Work",
    "img": "./graphs/bar_gender.png"
  },
  {
    "title": "Time Spent in Categories by Education",
    "img": "./graphs/bar_race.png"
  },
  {
    "title": "Time Spent in Categories by Leisure",
    "img": "./graphs/bar_inc.png"
  },
  {
    "title": "Time Spent in Categories by Personal Care",
    "img": "./graphs/bar_emp.png"
  }
] `;

const graphsJson = JSON.parse(graphsData)

// grab element with the id 'container'
const graphsContainer = document.getElementById("container");

// a function that adds all graphs data to the page
function addGraphsToPage(graphs) {
  graphs.forEach(graph => {
    let card = document.createElement('div');
    card.classList.add('card');
    card.innerHTML = `<img src='${graph.img}' alt='${graph.title}' />
                      <b>${graph.title}</b>`;
    graphsContainer.appendChild(card);
  });
}

addGraphsToPage(graphsJson);


