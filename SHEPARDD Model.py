# SHEPARDD E. granulosus sensu latp tranmsission model 
# Author: M. Entezami
import math
from enum import IntEnum
import numpy as np
from mesa import Agent, Model
from mesa.datacollection import DataCollector


class CEModel(Model):
    """
    A model for the transmission of E. granulosus sensu lato in dogs and sheep in a single farm setting. Values are fitted to best simulate trasmission in Rio Negro, Argentina
    """

    def __init__(
            self,
            Runid=0,
            steps=300,                                      #Number of steps the model runs for
            n_dogs=5,                                       #Total number of dog agents
            p_dog_inf = 0.4,                                
            p_sheep_inf = 0.6,
            n_sheep=700,                                    #Total number of Sheep agents with 1/7th in each cohort (aged 0-6) with an addition 1/7 aged 0 as male lambs
            scaling=1,                                      #Used to scale the population and transmission dynamics (Mostly for fitting so dog population is more stable)
            seed=None,
            egg_count=10000,                                #Initial egg count the the start of the run
            egg_viability=0.012,                            #Viable cyst proportion from eggs count (Roberts et al)
            cyst_fertility=10.68,                           #Larva fertility variable (Torgerson larva model)
            parasite_shedding=200,                          #From Torgeson age variation of larva paper
            parasite=100,                                   #Number of Parasites initally found in infected dogs
            proto_survivability=0.047,                      #Proportion of protoscoleces which survive to develop into tapeworms (Roberts et al)
            max_cyst_per_sheep=12,                          #Max cysts that can accumilate in a single sheep
            sheep_consumption_rate=1,                       #Proportion of eggs that the sheep eats (Fitting)
            dog_het = 1,       
            sheep_het = 1,                             
            dog_consumption_rate = 1,                       #Proportion of cysts eaten by dogs during offal feading (Fitting)
            s_death_rate= 1 - (0.9 ** (1/24)),              #Probablility of natural sheep death ever timestep
            cyst_latent_length=2,                           #Length of latent stage before cysts start to grow
            proto_latent_length=3,                          #Length of latent stage before protoscolex matures to adult worm
            egg_life_exp=72,                                #Life expectancy of the egg in the environment (Poissoned 36 months)
            parasite_life_exp=28,                           #Life expectancy of the parasite in dog intestines (Poissoned 6 - 22 months)
            # dog_immunity_rate = 0.3,
            # dog_immunity_loss = 0.02 ** 24,
            proto_availability=0,
            dead_sheep={},
            alive_dog=[],
            dog_het_list=[],
            yearly_sheep_death=0,
            biweekly_dead_dog=0,
            current_id=1,
            verbose = False,                               #Set to True for diagnostic information
            intervention = False,                          #Set to True for intervention implementation
            vacc_burning_period = 20,                      #Number of years after which vaccine interventions start
            dew_burning_period = 20,                       #Number of years after which deworming interventions start
            alt_burning_period = 20,                       #Number of years after which alternative interventions start
            dog_deworming_cov = 1,                         #Percentage of farms that recieve dog deworming each interval
            dog_deworming_cov_alt = 1,
            dog_deworming_annualfreq = 4,                  #Annual frequency of deworming
            sheep_vaccine_dose_eff = [0, 0.85, 0.85, 0.98],#Vaccine protection for each dosage amount [0 Doses, 1 Dose, 2 Doses, 3 Doses] 
            dog_deworming_schedule = [False],
            sheep_vaccine_cov = 1,                         #Percentage of farms that recieve sheep vaccination each interval
            sheep_vaccine_cov_alt = 1,
            sheep_vaccine_schedule = [False, False],
            vaccine_dose_count = 0,
            deworm_dose_count = 0
    ):

        self.Runid=Runid
        self.steps=steps
        self.num_agents_d=n_dogs
        self.num_agents_s=n_sheep
        self.p_dog_inf = p_dog_inf
        self.p_sheep_inf = p_sheep_inf
        self.scaling=scaling
        self.seed=seed
        self.egg_count=egg_count
        self.egg_viability=egg_viability
        self.cyst_fertility=cyst_fertility
        self.parasite_shedding=parasite_shedding
        self.parasite=parasite
        self.proto_availability=proto_availability
        self.proto_survivability=proto_survivability
        self.max_cyst_per_sheep=max_cyst_per_sheep
        self.sheep_consumption_rate=sheep_consumption_rate
        self.dog_het = dog_het
        self.sheep_het = sheep_het
        self.dog_consumption_rate = dog_consumption_rate
        self.s_death_rate=s_death_rate
        self.cyst_latent_length=cyst_latent_length
        self.proto_latent_length=proto_latent_length
        self.egg_life_exp=egg_life_exp
        self.parasite_life_exp=parasite_life_exp
        # self.dog_immunity_rate = dog_immunity_rate
        # self.dog_immunity_loss = dog_immunity_loss
        self.dead_sheep=dead_sheep
        self.alive_dog=alive_dog
        self.dog_het_list=dog_het_list
        self.yearly_sheep_death = yearly_sheep_death
        self.biweekly_dead_dog = biweekly_dead_dog
        self.current_id = current_id
        self.verbose = verbose
        self.schedule = RandomActivationByBreed(self)
        self.intervention = intervention
        self.vacc_burning_period = vacc_burning_period
        self.dew_burning_period = dew_burning_period
        self.alt_burning_period = alt_burning_period
        self.dog_deworming_cov = dog_deworming_cov
        self.dog_deworming_cov_alt = dog_deworming_cov_alt
        self.dog_deworming_annualfreq = dog_deworming_annualfreq
        self.dog_deworming_schedule = dog_deworming_schedule * dog_deworming_annualfreq
        self.sheep_vaccine_dose_eff = sheep_vaccine_dose_eff
        self.sheep_vaccine_cov = sheep_vaccine_cov
        self.sheep_vaccine_cov_alt = sheep_vaccine_cov_alt
        self.sheep_vaccine_schedule = sheep_vaccine_schedule
        self.vaccine_dose_count = vaccine_dose_count
        self.deworm_dose_count = deworm_dose_count
        
        if self.seed is not None:
            np.random.seed(self.seed)  # Set the provided seed value
        else:
            np.random.seed()

        # Create sheep
        age = 0
        for i in range(6):

            for j in range(math.floor(((self.num_agents_s * self.scaling * self.p_sheep_inf) / 6) + 0.5)):
                sheep = SheepAgent(("s" + str(self.next_id())), self)
                self.schedule.add(sheep)
                sheep.age = age
                sheep.cyst_year = np.array([int(np.random.uniform(low=1, high=2, size=1))]*(age+1))
                sheep.ind_het = np.random.gamma(self.sheep_het, 1/self.sheep_het)
            
            for k in range(math.floor(((self.num_agents_s * self.scaling * (1- self.p_sheep_inf)) / 6) + 0.5)):
                sheep = SheepAgent(("s" + str(self.next_id())), self)
                self.schedule.add(sheep)
                sheep.age = age
                sheep.ind_het = np.random.gamma(self.sheep_het, 1/self.sheep_het)
                
            age += 1
            
        
        # Create dog heterogenity value:
        for i in range(self.num_agents_d*self.scaling):
            self.dog_het_list.insert(0, np.random.gamma( self.dog_het, 1/self.dog_het))
        #print(self.dog_het_list)
        
        # Create dogs:
        #print (range(math.floor(self.num_agents_d * self.scaling * self.p_dog_inf + 0.5)))
        for i in range(math.floor(self.num_agents_d * self.scaling * self.p_dog_inf + 0.5)):
            dog = DogAgent(("d" + str(self.next_id())), self)
            self.schedule.add(dog)
            self.alive_dog.append(dog)
            dog.n_parasite = self.parasite
            dog.ind_het = self.dog_het_list.pop(0)
            #print(dog.ind_het)
        
        #print (range(math.floor(self.num_agents_d * self.scaling * (1 -self.p_dog_inf) + 0.5)))
        for i in range(math.floor(self.num_agents_d * self.scaling * (1 - self.p_dog_inf) + 0.5)):
            dog = DogAgent(("d" + str(self.next_id())), self)
            self.schedule.add(dog)
            self.alive_dog.append(dog)
            dog.ind_het = self.dog_het_list.pop(0)
            #print(dog.ind_het)
        
        self.running = True
        
        #Set the model and agent parametes to be recorded
        self.datacollector = DataCollector(model_reporters={"Egg Count": "egg_count",
                                                            "Seed": "seed",
                                                            "Vaccine Dosage Count": "vaccine_dose_count",
                                                            "Deworm Dosage Count": "deworm_dose_count"},
            agent_reporters={"State": "state",
                             "Species": "species",
                             "Cyst age list": "cyst_year",
                             "Latent cyst list": "cyst_L",
                             "Age": "age",
                             "N parasite": "n_parasite",
                             })
        
    def replenish_livestock(self):
        """
        Replenishes the animals that have died by new stock
        """
        if ((self.schedule.time % 24) + 1) == 18:
            for i in range(math.floor(self.yearly_sheep_death / 2)):
                sheep = SheepAgent(("s" + str(self.next_id())), self)
                self.schedule.add(sheep)
                # self.s_interations.append(sheep)
                sheep.age = 0
                sheep.ind_het = np.random.gamma(self.sheep_het, 1/self.sheep_het)

            for i in range(math.floor((self.yearly_sheep_death / 2) + 0.5)):
                sheep = SheepAgent(("s" + str(self.next_id())), self)
                self.schedule.add(sheep)
                # self.s_interations.append(sheep)
                sheep.age = 0
                sheep.ind_het = np.random.gamma(self.sheep_het, 1/self.sheep_het)
                sheep.type = "meat"

            self.yearly_sheep_death = 0

        for i in range(self.biweekly_dead_dog):
            dog = DogAgent(("d" + str(self.next_id())), self)
            self.schedule.add(dog)
            self.alive_dog.append(dog)
            dog.age = 0
            dog.ind_het = self.dog_het_list.pop(0)
            #print(dog.ind_het)
    
    def egg_decay(self):
        """
        Simulates the rate at which eggs die
        """
        self.egg_count = max([0,self.egg_count - np.random.poisson((self.egg_count * (1 / self.egg_life_exp)), None)])
                
    def intervention_coverage(self):
        if (self.schedule.time+1) < (self.alt_burning_period * 24) :
            self.sheep_vaccine_schedule = [np.random.choice([False, True], p=[(1-self.sheep_vaccine_cov), self.sheep_vaccine_cov]),
                                           np.random.choice([False, True], p=[(1-self.sheep_vaccine_cov), self.sheep_vaccine_cov])]
            self.dog_deworming_schedule = [np.random.choice([False, True], p=[(1-self.dog_deworming_cov), self.dog_deworming_cov]),
                                           np.random.choice([False, True], p=[(1-self.dog_deworming_cov), self.dog_deworming_cov]),
                                           np.random.choice([False, True], p=[(1-self.dog_deworming_cov), self.dog_deworming_cov]),
                                           np.random.choice([False, True], p=[(1-self.dog_deworming_cov), self.dog_deworming_cov])]
        else:
            self.sheep_vaccine_schedule = [np.random.choice([False, True], p=[(1-self.sheep_vaccine_cov_alt), self.sheep_vaccine_cov_alt]),
                                           np.random.choice([False, True], p=[(1-self.sheep_vaccine_cov_alt), self.sheep_vaccine_cov_alt])]
            self.dog_deworming_schedule = [np.random.choice([False, True], p=[(1-self.dog_deworming_cov_alt), self.dog_deworming_cov_alt]),
                                           np.random.choice([False, True], p=[(1-self.dog_deworming_cov_alt), self.dog_deworming_cov_alt]),
                                           np.random.choice([False, True], p=[(1-self.dog_deworming_cov_alt), self.dog_deworming_cov_alt]),
                                           np.random.choice([False, True], p=[(1-self.dog_deworming_cov_alt), self.dog_deworming_cov_alt])]

    def step(self):
        """
        Advance the model by one step.
        """
        self.replenish_livestock()
        self.biweekly_dead_dog = 0
        if ((self.schedule.time) % 24) == 0:
            self.intervention_coverage()
        self.proto_availability = self.proto_availability = sum(sum(val) if isinstance(val, list) else val for val in self.dead_sheep.values())
        self.dead_sheep = {}
        self.schedule.step()
        self.proto_availability = 0
        self.egg_decay()
        # Collect data
        self.datacollector.collect(self)
        if self.verbose:
            print(self.schedule.time +1)
            print((self.schedule.time % 24)+1)
            print("egg count :" + str(self.egg_count))
            print("dead sheep per time-step :" + str(len(self.dead_sheep)))
            print("dead sheep per year :" + str(self.yearly_sheep_death))
        if self.schedule.time >= self.steps:
            self.running = False


from collections import defaultdict

from mesa.time import RandomActivation


class RandomActivationByBreed(RandomActivation):
    """
    A scheduler which activates each type of agent once per step, in random
    order, with the order reshuffled every step.
    This is equivalent to the NetLogo 'ask breed...' and is generally the
    default behavior for an ABM.
    Assumes that all agents have a step() method.
    """

    def __init__(self, model):
        super().__init__(model)
        self.agents_by_breed = defaultdict(dict)

    def add(self, agent):
        """
        Add an Agent object to the schedule
        Args:
            agent: An Agent to be added to the schedule.
        """

        self._agents[agent.unique_id] = agent
        agent_class = type(agent)
        self.agents_by_breed[agent_class][agent.unique_id] = agent

    def remove(self, agent):
        """
        Remove all instances of a given agent from the schedule.
        """

        del self._agents[agent.unique_id]

        agent_class = type(agent)
        del self.agents_by_breed[agent_class][agent.unique_id]

    def step(self, by_breed=True):
        """
        Executes the step of each agent breed, one at a time, in random order.
        Args:
            by_breed: If True, run all agents of a single breed before running
                      the next one.
        """
        if by_breed:
            for agent_class in self.agents_by_breed:
                self.step_breed(agent_class)
            self.steps += 1
            self.time += 1
        else:
            super().step()

    def step_breed(self, breed):
        """
        Shuffle order and run all agents of a given breed.
        Args:
            breed: Class object of the breed to run.
        """
        agent_keys = list(self.agents_by_breed[breed].keys())
        self.model.random.shuffle(agent_keys)
        for agent_key in agent_keys:
            self.agents_by_breed[breed][agent_key].step()

    def get_breed_count(self, breed_class):
        """
        Returns the current number of agents of certain breed in the queue.
        """
        return len(self.agents_by_breed[breed_class].values())


class State(IntEnum):
    SUSCEPTIBLE = 0
    LATENT = 1
    INFECTED = 2
    RECOVERED = 3
    DEAD = 4

class SheepAgent(Agent):
    """ 
    Properties of the Sheep agent
    """
    def __init__(self, unique_id, model):
        super().__init__(unique_id, model)
        self.age = self.random.normalvariate(1.5, 0.5)
        self.species = "sheep"
        self.b_week_age = 0
        self.state = State.SUSCEPTIBLE
        self.cyst_L = [0] * self.model.cyst_latent_length
        self.cyst_year = np.array([0])
        self.ind_het = "null"
        self.n_parasite = 0
        self.type = "wool"
        self.culling_date = 0
        self.dead = False
        self.denisty_dependance = 1
        self.vaccine_dose = 0
        # self.agent_egg = 0
        # self.ind_infect_chance = 0
        # self.infection_time = 0

    def graze(self):
        """
        Uptake of eggs and infection
        
        """

        eggs_per_sheep = self.model.egg_count / self.model.schedule.get_breed_count(SheepAgent)
        eggs_consumed = min([eggs_per_sheep,(eggs_per_sheep * self.model.sheep_consumption_rate) * self.ind_het * (1-self.model.sheep_vaccine_dose_eff[self.vaccine_dose])])
        #print("egg:" + str(eggs_consumed))
        #print("vacc:" + str((1-self.model.sheep_vaccine_dose_eff[self.vaccine_dose])))
        self.model.egg_count = max([0, self.model.egg_count - eggs_consumed])
        self.cyst_L.insert(0, eggs_consumed)
        if self.state != State.INFECTED and self.state != State.LATENT:
            if sum(self.cyst_L) >= 1:
                self.state = State.LATENT

    def cyst(self):
        """
        For now this section just delays the development of the cyst from latent to infective by 2 timesteps (1 month)
        """
        if (sum(self.cyst_year)) >= self.model.max_cyst_per_sheep:
            self.density_dependance = 0
            
        else:
            self.density_dependance = 1 - ((sum(self.cyst_year)) / self.model.max_cyst_per_sheep)
        
        #print(self.cyst_L)
        self.cyst_year[0] += np.random.poisson(((self.cyst_L.pop() * self.model.egg_viability) * self.density_dependance),
                                         None)
        
        if (sum(self.cyst_year)) > self.model.max_cyst_per_sheep:
            self.cyst_year[0] = (self.model.max_cyst_per_sheep - sum(self.cyst_year)) + self.cyst_year[0]
        
        if (sum(self.cyst_year)) >= 1:
            self.state = State.INFECTED
        if (sum(self.cyst_year)) < 1:
            self.state = State.SUSCEPTIBLE

    def culling(self):
        """
        Culling of sheep during specific months of the year
        The young lambs are culled randomly during the winter months and the sheep sheep older than 6 killed during the rest
        """
        if self.type == "meat" and self.culling_date == 0:
            if self.age >= 1:
                season = np.random.choice([0, 1], p=[(1/2), (1/2)])
                if season == 1:
                    self.culling_date = math.floor((self.random.uniform(1, 4)) + 0.5)
                else:
                    self.culling_date = math.floor((self.random.uniform(22, 24)) + 0.5)

        if self.age >= 6 and self.culling_date == 0:
            self.culling_date = math.floor((self.random.uniform(6, 23)) + 0.5)

        if self.culling_date == ((self.model.schedule.time % 24) + 1):
            if self.state == State.INFECTED:
                self.cyst_year = np.insert(self.cyst_year, 0, 0)

            self.model.yearly_sheep_death += 1  
            self.model.schedule.remove(self)
            cyst_proto = []
            #print(self.cyst_year)
            for cyst_age in range(len(self.cyst_year)):
                for n_cyst in range(int(self.cyst_year[cyst_age])):
                    cyst_proto.insert(0, self.model.cyst_fertility * ((cyst_age + 1) ** 3))
            self.model.dead_sheep[self.unique_id] = cyst_proto
            self.dead = True
            
    def natural_death(self):
        """
        Natural Death and Infectious Death
        """

        if self.dead == False:
            death = np.random.choice([0, 1], p=[(1 - self.model.s_death_rate), (self.model.s_death_rate)])
            if death == 1:
                self.model.yearly_sheep_death += 1
                cyst_proto = []
                #print(self.cyst_year)
                for cyst_age in range(len(self.cyst_year)):
                    for n_cyst in range(int(self.cyst_year[cyst_age])):
                        cyst_proto.insert(0, self.model.cyst_fertility * ((cyst_age + 1) ** 3))
                self.model.dead_sheep[self.unique_id] = cyst_proto
                self.model.schedule.remove(self)
        
    def vaccination(self):
        """
        Vaccination of sheeo according to schedule and dosage
        """
        if math.floor((((self.model.schedule.time + 1) % 24) / 2) + 0.5)  == 12:
            if self.vaccine_dose == 0 and self.model.sheep_vaccine_schedule[0]:
                #print("vaccine")
                self.vaccine_dose = 1
                self.model.vaccine_dose_count += 1
            elif self.vaccine_dose == 2 and self.model.sheep_vaccine_schedule[0]:
                #print("vaccine")
                self.vaccine_dose = 3
                self.model.vaccine_dose_count += 1
        
        if math.floor((((self.model.schedule.time + 1) % 24) / 2) + 0.5)  == 3 and self.model.sheep_vaccine_schedule[1]:
            if self.vaccine_dose == 1:
                #print("vaccine")
                self.vaccine_dose = 2
                self.model.vaccine_dose_count += 1

    def ageing(self):
        self.b_week_age += 1
        if self.b_week_age >= 24:
            self.age += 1
            self.b_week_age = 0
            self.cyst_year = np.insert(self.cyst_year, 0, 0)
            #print("cyst list:" + str(self.cyst_year))

    def step(self):
        self.cyst()
        self.graze()
        self.culling()
        self.natural_death()
        self.ageing()
        if self.model.intervention and (self.model.schedule.time+1) > (self.model.vacc_burning_period * 24):
            self.vaccination()


class DogAgent(Agent):
    """ 
    Properties of the Dog agent
    """
    def __init__(self, unique_id, model):
        super().__init__(unique_id, model)
        self.age = round(self.random.uniform(10, 110))  # between 10 and 110 fortnights
        self.ind_het = "null" 
        self.species = "dog"
        self.b_week_age = 0
        self.new_parasite = 0
        self.state = State.SUSCEPTIBLE
        self.n_parasite = 0
        self.latent_prot = [0] * (self.model.proto_latent_length - 1)
        self.agent_proto = 0
        self.immune = 0
        self.type = ""
        self.cyst_L = 0
        self.cyst_year = 0
        # self.infection_time = 0
        # self.egg_count = egg_count

    def shed(self):
        """
        Shed eggs into the environment
        """
        self.model.egg_count += sum(
            np.random.poisson(self.model.parasite_shedding, self.n_parasite))  # based on number of eggs per 15 days

    def parasite(self):
        """
        Death of adult parasite population
        """
        self.n_parasite = max([0, self.n_parasite - np.random.poisson(self.n_parasite * (
                1 / self.model.parasite_life_exp))])  #6-22 months life expectancy

        """
        Growth of juvenile parasites with only a proportion surviving the maturation proces to adult parasites
        """
        def proto_decay(arr):
            new_arr = [0] * (self.model.proto_latent_length - 1)
            for counter in range(len(arr)):
                if arr[counter] > 0:
                    new_arr[counter] = arr[counter]
                else:
                    new_arr[counter] = 0
            return new_arr

        self.latent_prot = proto_decay(self.latent_prot)
        #print("latent 1 is:" + str(self.latent_prot))
        self.new_parasite = self.latent_prot.pop()
        if self.new_parasite > 0:
            self.n_parasite += np.random.poisson((self.new_parasite * (self.model.proto_survivability)), None)
        if self.n_parasite > 0:
            self.state = State.INFECTED
        
        
        else:
            self.state = State.SUSCEPTIBLE
        #print("latent 2 is:" + str(self.latent_prot))
        #print("new is:" + str(self.new_parasite))
        
        
        #print("agent_proto at latent is" + str(self.agent_proto))        
        self.latent_prot.insert(0, math.floor(self.agent_proto))
        self.agent_proto = 0

    def infect(self):
        """
        Eating cysts and becoming infected.
        
        The agent looks at the number of protoscolex available from dead infected sheep from the time step and calculated how many protoscoleces per dog
        is available for that time step.
        """

        self.agent_proto += min([self.model.proto_availability / self.model.schedule.get_breed_count(DogAgent),
                                 self.model.proto_availability / self.model.schedule.get_breed_count(DogAgent)  * self.model.dog_consumption_rate * self.ind_het])
        #print("agent_proto at infect is" + str(self.agent_proto)) 

        #immunity component of infection (depreciated)
        """if self.immune == 0:
            self.agent_cyst = self.model.proto_availability / self.model.schedule.get_breed_count(DogAgent)
            self.Latent.insert(0, self.agent_cyst * self.model.proto_survivability)
            if self.agent_cyst >= 1:
                imm_gain = np.random.choice([0,1], p=[(1 - self.model.dog_immunity_rate),(self.model.dog_immunity_rate)])
                if imm_gain == 1:
                    self.immune = 1"""

    #immunity component (depreciated)
    """def immunity(self):
        if self.immune == 1:
            imm_loss = np.random.choice([0,1], p=[(1 - self.model.dog_immunity_loss),(self.model.dog_immunity_loss)])
            if imm_loss == 1:
                 self.immune = 0"""

    def deworm(self):
        """
        Deworming according to schedule, removing all worms in dog host
        """

        #print("deworm")
        self.n_parasite = int(0)
        self.latent_prot = [int(x * 0) for x in self.latent_prot]
        self.model.deworm_dose_count += 1

    def ageing(self):
        """
        Agent Aging and retirement
        """
        self.age += 1

        if self.age > 156:  # dogs older than 6 years retire
            self.model.biweekly_dead_dog += 1
            self.model.schedule.remove(self)
            self.model.alive_dog.remove(self)
            self.model.dog_het_list.insert(0, self.ind_het)

    def step(self):
        # print("dog")
        # print(self.agent_cyst)
        if self.model.intervention and round((self.model.schedule.time + 1) % (24 / (self.model.dog_deworming_annualfreq))) == 0 and (self.model.schedule.time+1) > (self.model.dew_burning_period * 24) and self.model.dog_deworming_schedule[(self.model.schedule.time//6) % 4]:
            self.deworm()
        self.shed()
        self.infect()
        self.parasite()
        # self.immunity()
        self.ageing()