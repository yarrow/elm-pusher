describe('Sign-in tests', () => {
  it('Redirects to main page only after name and (correct) password are entered', () => {
    cy.visit('/')
    cy.location('pathname').should('eq', '/sign-in')

    // Non-blank name is required
    cy.get('[data-cy=name]').clear()
    cy.get('[data-cy=sign-in]').click()
    cy.get('[data-cy=error]').contains(/name/i)
    cy.location('pathname').should('eq', '/sign-in')

    // Will be trimmed and capitalized, to 'Yar Row'
    cy.get('[data-cy=name]').type('     Yar row        ')
    cy.get('[data-cy=password]').clear()

    // Non-blank password is required
    cy.get('[data-cy=sign-in]').click()
    cy.get('[data-cy=error]').contains(/password/i)
    cy.location('pathname').should('eq', '/sign-in')

    // Bad password lands up back on the Sign-In page with an error message
    cy.get('[data-cy=password]').type('Terrible, Horrible, No Good, Very Bad Password')
    cy.get('[data-cy=sign-in]').click()
    cy.location('pathname').should('eq', '/sign-in')
    cy.get('[data-cy=error]').contains(/password/i)

    // The right password sends us to the home page
    cy.get('[data-cy=password]').clear()
    cy.get('[data-cy=password]').type(Cypress.env('PASSWORD'))
    cy.get('[data-cy=sign-in]').click()
    cy.location('pathname').should('eq', '/')
    cy.contains('Yar Row')
  })
})
