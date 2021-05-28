describe('Sign-in tests', () => {
  it('Redirects to main page only after name and password are entered', () => {
    cy.visit('http://localhost:8888/')
    cy.location('pathname').should('eq', '/sign-in')

    cy.get('[data-cy=name]').clear()
    cy.location('pathname').should('eq', '/sign-in')

    cy.get('[data-cy=sign-in]').click()
    cy.get('[data-cy=error]').contains(/name/i)
    cy.location('pathname').should('eq', '/sign-in')

    cy.get('[data-cy=name]').type('     Yarrow        ') // Will be trimmed to 'Yarrow'
    cy.get('[data-cy=password]').clear()
    cy.get('[data-cy=sign-in]').click()
    cy.get('[data-cy=error]').contains(/password/i)
    cy.location('pathname').should('eq', '/sign-in')

    cy.get('[data-cy=password]').type('any-damn-password')
    cy.get('[data-cy=sign-in]').click()
    cy.location('pathname').should('eq', '/')
    cy.contains('Yarrow')
  })
})
